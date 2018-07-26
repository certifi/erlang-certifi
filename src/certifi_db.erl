%% @private
-module(certifi_db).
-behaviour(gen_server).

-compile({parse_transform, ct_expand}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/0,
    cacertfile/0
   ]).

-ignore_xref(
   [start_link/0
   ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

cacertfile() ->
    try ets:lookup(?TABLE, cacert_filepath) of
        [{_, CacertFilePath}] ->
            CacertFilePath;
        [] ->
            % not updated yet
            bundled_cacertfile()
    catch
        error:badarg ->
            % not runnign yet (probably compiling)
            bundled_cacertfile()
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    _ = create_table(),
    _ = schedule_auto_update(now),
    {ok, #state{
           }}.

handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

handle_info(try_updating, State) ->
    {ok, Url} = application:get_env(certifi, auto_update_url),
    {ok, SslOpts} = certifi_https:secure_ssl_opts(Url),
    Method = get,
    Headers = [{"accept", "application/x-pem-file"}],
    Request = {Url, Headers},
    Timeout = timer:seconds(10),
    HttpOptions = [{ssl, SslOpts}, {timeout, Timeout}],
    OutputFilePath = generate_update_filepath(),
    ok = filelib:ensure_dir(OutputFilePath),
    Options = [{stream, OutputFilePath}],
    _ = case httpc:request(Method, Request, HttpOptions, Options) of
            {ok, saved_to_file} ->
                io:format("updated (~p)~n", [OutputFilePath]),
                ets:insert(?TABLE, {cacert_filepath, OutputFilePath}),
                recompile_certifi_module();
            {error, Reason} ->
                io:format("failed to update: ~p~n", [Reason])
        end,
    schedule_auto_update(later),
    {noreply, State};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions - certifi Module Regeneration
%% ------------------------------------------------------------------

create_table() ->
    TableOpts = [named_table, protected, {read_concurrency,true}],
    ets:new(?TABLE, TableOpts).

certifi_module_source() ->
    ct_expand:term(
      (fun () ->
               LibDir = code:lib_dir(certifi),
               ModulePath = filename:join([LibDir, "src", "certifi.erl"]),
               {ok, ModuleSource} = file:read_file(ModulePath),
               ModuleSource
       end)()).

certifi_compile_options() ->
    Info = certifi:module_info(compile),
    AllOptions = proplists:get_value(options, Info, []),
    FilteredOptions =
        lists:filter(
          fun ({K, _V}) when is_atom(K) ->
                  not lists:member(K, [i, outdir]);
              (K) when is_atom(K) ->
                  true
          end,
          AllOptions),
    %%
    [{parse_transform, ct_expand} | FilteredOptions].

bundled_cacertfile() ->
  PrivDir = case code:priv_dir(certifi) of
              {error, _} ->
                %% try to get relative priv dir. useful for tests.
                AppDir = filename:dirname(
                           filename:dirname(code:which(?MODULE))
                          ),
                filename:join(AppDir, "priv");
              Dir -> Dir
            end,
  filename:join(PrivDir, "cacerts.pem").

recompile_certifi_module() ->
    ModuleSource = certifi_module_source(),
    CompileOptions = certifi_compile_options(),

    % based on: https://stackoverflow.com/a/2160696
    %
    % create token groups
    ModuleSourceStr = unicode:characters_to_list(ModuleSource),
    {ok, TokenList, _} = erl_scan:string(ModuleSourceStr),
    TokenGroups =
        lists_split_on(
          fun ({dot, _Line}) -> true;
              (_) -> false
          end,
          TokenList),

    % create form groups
    FormsGroups =
        lists:map(
          fun (TokenGroup) ->
                  {ok, FormsGroup} = erl_parse:parse_form(TokenGroup),
                  FormsGroup
          end,
          TokenGroups),

    % compile form groups to binary
    {ok, certifi, ByteCode} = compile:forms(FormsGroups, CompileOptions),

    % load module from binary
    {module, certifi} = code:load_binary(certifi, "nofile", ByteCode).

lists_split_on(Fun, List) ->
    lists_split_on_recur(Fun, List, [], []).

lists_split_on_recur(Fun, [H|T], GroupAcc, GroupsAcc) ->
    case Fun(H) of
        true ->
            OrderedGroupAcc = lists:reverse([H | GroupAcc]),
            lists_split_on_recur(Fun, T, [], [OrderedGroupAcc | GroupsAcc]);
        false ->
            lists_split_on_recur(Fun, T, [H | GroupAcc], GroupsAcc)
    end;
lists_split_on_recur(_Fun, [], GroupAcc, GroupsAcc) ->
    OrderedGroupAcc = lists:reverse(GroupAcc),
    lists:foldl(
      fun ([], Acc) ->
              Acc;
          (Group, Acc) ->
              [Group | Acc]
      end,
      [],
      [OrderedGroupAcc | GroupsAcc]).

%% ------------------------------------------------------------------
%% Internal Function Definitions - Auto Updating
%% ------------------------------------------------------------------

schedule_auto_update(now) ->
    self() ! try_updating;
schedule_auto_update(later) ->
    {ok, Interval} = application:get_env(certifi, auto_update_interval),
    IntervalMs = timer:seconds(Interval),
    erlang:send_after(IntervalMs, self(), try_updating).

generate_update_filepath() ->
    <<RandomUint128:128/integer>> = crypto:strong_rand_bytes(16),
    Filename = "cacerts." ++ integer_to_list(RandomUint128, 36) ++ ".pem",
    Directory = filename:basedir(user_cache, "certifi"),
    filename:join(Directory, Filename).

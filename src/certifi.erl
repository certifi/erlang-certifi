-module(certifi).
%% Causes the parse transformation function Module:parse_transform/2
%% to be applied to the parsed code before the code is checked for errors.
-compile({parse_transform, certifi_pt}).

-export([cacertfile/0,
         cacerts/0,
         eu_cacertfile/0,
         eu_cacerts/0]).

%% Only used for bundles that are not embedded at compile time.
-compile({nowarn_unused_function, [load/2]}).

%% @doc CACertFile gives the path to the file with an X.509 certificate list
%% containing the Mozilla CA Certificate.
%%
%% The file is used via the `cacertfile' setting in {@link //ssl/ssl} options,
%% passed to the `ssl:connect/2,3,4' function.
-spec cacertfile() -> Result when
	Result :: file:filename_all().
cacertfile() ->
  filename:join(priv_dir(), "cacerts.pem").

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate.
%%
%% The certificate list is used via the `cacerts' setting in {@link //ssl/ssl}
%% options, passed to the `connect/2,3,4' function.
%%
%% The list is embedded in the module at compile time unless certifi was
%% compiled with a `CERTIFI_EMBED' setting that leaves out `mozilla'. It is
%% then read from {@link cacertfile/0} on first use and cached.
-spec cacerts() -> [binary(),...].
cacerts() ->
    load(cacerts, cacertfile()).

%% @doc Path to the file with the CA certificates for qualified website
%% authentication (QWAC) taken from the EU trusted lists.
%%
%% Many of these certificates are issuing CAs rather than roots. See the
%% "EU trusted lists" guide for how to use them with {@link //ssl/ssl}.
-spec eu_cacertfile() -> Result when
	Result :: file:filename_all().
eu_cacertfile() ->
  filename:join(priv_dir(), "eu_cacerts.pem").

%% @doc DER encoded CA certificates for qualified website authentication
%% (QWAC) taken from the EU trusted lists.
%%
%% By default the list is read from {@link eu_cacertfile/0} on first use and
%% cached. Compile certifi with a `CERTIFI_EMBED' setting that includes `eu'
%% to embed it in the module instead.
-spec eu_cacerts() -> [binary(),...].
eu_cacerts() ->
    load(eu_cacerts, eu_cacertfile()).

%% Same order as the embedded lists, so results do not depend on how certifi
%% was compiled.
load(Key, File) ->
    case persistent_term:get({?MODULE, Key}, undefined) of
        undefined ->
            {ok, Pem} = file:read_file(File),
            Certs = lists:reverse(
                      [Der || {'Certificate', Der, not_encrypted} <- public_key:pem_decode(Pem)]),
            persistent_term:put({?MODULE, Key}, Certs),
            Certs;
        Certs ->
            Certs
    end.

priv_dir() ->
  case code:priv_dir(certifi) of
    {error, _} ->
      %% try to get relative priv dir. useful for tests.
      AppDir = filename:dirname(
                 filename:dirname(code:which(?MODULE))
                ),
      filename:join(AppDir, "priv");
    Dir -> Dir
  end.

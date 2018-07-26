%% @private
-module(certifi_app).
-behaviour(application).

%% ------------------------------------------------------------------
%% application Function Exports
%% ------------------------------------------------------------------

-export(
   [start/2,
    stop/1
   ]).

%% ------------------------------------------------------------------
%% application Function Definitions
%% ------------------------------------------------------------------

-spec start(term(), list()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    certifi_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

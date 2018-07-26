%% @private
-module(certifi_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).

-define(CHILD_SPEC(Module, Type),
        {Module,                   % id
         {Module, start_link, []}, % start
         permanent,                % restart
         5000,                     % shutdown
         Type,
         [Module]}).               % modules

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    SupFlags = {one_for_one, 0, 1},
    ChildSpecs = [?CHILD_SPEC(certifi_db, worker)],
    {ok, {SupFlags, ChildSpecs}}.

-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).

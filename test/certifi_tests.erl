-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    Certs = [Cert1, Cert2, Cert3 | _] = certifi:cacerts(),
    [?_assertEqual(135, length(Certs))
    ,?_assertMatch(<<48,130,6,91,48,130,4,67,160,3,2,1,2,2,17,0,202,233,27,137,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,5,90,48,130,3,66,160,3,2,1,2,2,16,79,210,43,143,245,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,5,70,48,130,3,46,160,3,2,1,2,2,16,93,223,177,218,90, _/binary>>, Cert3)
    ,?_assertMatch(<<48,130,3,117,48,130,2,93,160,3,2,1,2,2,11,4,0,0,0,0,1,21,75,90,195,148,48,13,6,_/binary>>, lists:last(Certs))
    ].

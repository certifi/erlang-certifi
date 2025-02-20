-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    Certs = [Cert1, Cert2, Cert3 | _] = certifi:cacerts(),
    [?_assertEqual(150, length(Certs))
    ,?_assertMatch(<<48,130,5,169,48,130,3,145,160,3,2,1,2,2,16,105,38,9,126,128,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,5,169,48,130,3,145,160,3,2,1,2,2,16,115,59,48,4,72,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,2,35,48,130,1,169,160,3,2,1,2,2,20,22,21,199,195,216,_/binary>>, Cert3)
    ,?_assertMatch(<<48,130,3,117,48,130,2,93,160,3,2,1,2,2,11,4,0,0,0,0,1,21,75,90,195,148,48,13,6,_/binary>>, lists:last(Certs))
    ].

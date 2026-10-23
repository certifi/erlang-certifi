-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    Certs = [Cert1, Cert2, Cert3 | _] = certifi:cacerts(),
    [?_assertEqual(118, length(Certs))
    ,?_assertMatch(<<48,130,5,130,48,130,3,106,160,3,2,1,2,2,15,1,139,210,80,171,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,2,50,48,130,1,183,160,3,2,1,2,2,15,1,139,210,34,84,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,2,76,48,130,1,209,160,3,2,1,2,2,9,0,129,122,44,239,_/binary>>, Cert3)
    ,?_assertMatch(<<48,130,2,137,48,130,2,15,160,3,2,1,2,2,16,31,71,175,170,98,_/binary>>, lists:last(Certs))
    ].

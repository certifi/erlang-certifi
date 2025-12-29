-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    Certs = [Cert1, Cert2, Cert3 | _] = certifi:cacerts(),
    [?_assertEqual(143, length(Certs))
    ,?_assertMatch(<<48,130,5,131,48,130,3,107,160,3,2,1,2,2,16,85,165,217,103,148,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,2,53,48,130,1,186,160,3,2,1,2,2,16,35,249,195,214,53,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,5,147,48,130,3,123,160,3,2,1,2,2,20,67,250,12,95,78,_/binary>>, Cert3)
    ,?_assertMatch(<<48,130,4,145,48,130,3,121,160,3,2,1,2,2,4,69,107,80,84,48,_/binary>>, lists:last(Certs))
    ].

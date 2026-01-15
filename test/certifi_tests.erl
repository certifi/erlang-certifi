-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    %% Checking the contents is difficult because they change frequently.
    %% Therefore, this test only checks that certificates are loaded.
    Certs = certifi:cacerts(),
    [?_assert(length(Certs) > 0)
    ,?_assert(lists:all(fun is_binary/1, Certs))
    ].

cacerts_test_data_test_() ->
    Certs = [Cert1, Cert2, Cert3] = certifi:cacerts_test_data(),
    [?_assertEqual(3, length(Certs))
    ,?_assertMatch(<<48,130,3,199,48,130,2,175,160,3,2,1,2,2,20,46,59,44,50,129,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,3,199,48,130,2,175,160,3,2,1,2,2,20,22,71,8,124,36,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,3,199,48,130,2,175,160,3,2,1,2,2,20,39,199,152,45,116,_/binary>>, Cert3)
    ].

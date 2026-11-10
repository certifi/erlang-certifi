-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

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

cacertfile_matches_embedded_bundle_test() ->
    {ok, Pem} = file:read_file(certifi:cacertfile()),
    Certs = [Der || {'Certificate', Der, _} <- public_key:pem_decode(Pem)],
    ?assertEqual(lists:sort(Certs), lists:sort(certifi:cacerts())).

distrusted_emsign_roots_test_() ->
    Fingerprints = [crypto:hash(sha256, Cert) || Cert <- certifi:cacerts()],
    %% Removed by Mozilla NSS in October 2026 (bugs 2083709 and 2084101).
    Distrusted = [
        {"emSign Root CA - C1",
         <<16#125609aa301da0a249b97a8239cb6a34216f44dcac9f3954b14292f2e8c8608f:256>>},
        {"emSign ECC Root CA - C3",
         <<16#bc4d809b15189d78db3e1d8cf4f9726a795da1643ca5f1358e1ddb0edc0d7eb3:256>>},
        {"emSign ECC Root CA - G3",
         <<16#86a1ecba089c4a8d3bbe2734c612ba341d813e043cf9e8a862cd5c57a36bbe6b:256>>}
    ],
    [{Name, ?_assertNot(lists:member(Fingerprint, Fingerprints))}
     || {Name, Fingerprint} <- Distrusted].

eu_cacertfile_matches_eu_cacerts_test() ->
    {ok, Pem} = file:read_file(certifi:eu_cacertfile()),
    Certs = [Der || {'Certificate', Der, _} <- public_key:pem_decode(Pem)],
    ?assert(length(Certs) > 0),
    ?assertEqual(lists:sort(Certs), lists:sort(certifi:eu_cacerts())).

eu_cacerts_are_unique_ca_certificates_test() ->
    Certs = certifi:eu_cacerts(),
    ?assertEqual(length(Certs), length(lists:usort(Certs))),
    %% OTP 21 and 22 cannot decode RSA-PSS signed certificates; ssl ignores
    %% those, so only check the ones this release can decode.
    Decoded = [Otp || Der <- Certs, Otp <- decode(Der)],
    ?assert(length(Decoded) > 0),
    [?assert(is_ca(Otp)) || Otp <- Decoded].

decode(Der) ->
    try [public_key:pkix_decode_cert(Der, otp)]
    catch _:_ -> []
    end.

eu_cacerts_entries_have_metadata_test() ->
    {ok, Pem} = file:read_file(certifi:eu_cacertfile()),
    Blocks = tl(binary:split(Pem, <<"# Territory: ">>, [global])),
    ?assertEqual(length(certifi:eu_cacerts()), length(Blocks)),
    [?assertMatch({_, _}, binary:match(B, <<"# SHA256: ">>)) || B <- Blocks].

is_ca(#'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{extensions = Exts}}) ->
    lists:any(fun(#'Extension'{extnID = ?'id-ce-basicConstraints',
                               extnValue = #'BasicConstraints'{cA = CA}}) -> CA;
                 (_) -> false
              end, Exts).

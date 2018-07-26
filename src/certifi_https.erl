%% @private
-module(certifi_https).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [secure_ssl_opts/1,
    partial_chain/1
   ]).

-spec secure_ssl_opts(string())
        -> {ok, [ssl:option(), ...]} | {error, term()}.
secure_ssl_opts(Url) ->
    try http_uri:parse(Url) of
        {ok, {https, _UserInfo, Host, _Port, _Path, _Query}} ->
            Opts = host_ssl_opts(Host),
            {ok, Opts};
        {ok, {https, _UserInfo, Host, _Port, _Path, _Query, _Fragment}} ->
            Opts = host_ssl_opts(Host),
            {ok, Opts};
        %%
        {ok, {http, _UserInfo, _Host, _Port, _Path, _Query}} ->
            {error, insecure_transport};
        {ok, {http, _UserInfo, _Host, _Port, _Path, _Query, _Fragment}} ->
            {error, insecure_transport};
        {error, Reason} ->
            {error, Reason}
    catch
        Class:Reason ->
            {error, {bad_uri, {Class, Reason}}}
    end.

-spec partial_chain([public_key:der_encoded()])
        -> {trusted_ca, #'Certificate'{}} | unknown_ca.
partial_chain(Certs) ->
    % Taken from hackney, licensed under Apache 2 license,
    % which in turn took it from rebar3, licensed under BSD.
    Certs1 = lists:reverse([{Cert, public_key:pkix_decode_cert(Cert, otp)} ||
                            Cert <- Certs]),
    CACerts = certifi:cacerts(),
    CACerts1 = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CACerts],

    case lists_anymap(
           fun ({_, Cert}) ->
                   check_cert(CACerts1, Cert)
           end,
           Certs1)
    of
        {true, Trusted} ->
            {trusted_ca, element(1, Trusted)};
        false ->
            unknown_ca
    end.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

host_ssl_opts(Host) ->
    % Taken from hackney, licensed under Apache 2 license.
    CACerts = certifi:cacerts(),
    VerifyFun =
        {fun ssl_verify_hostname:verify_fun/3,
         [{check_hostname, Host}]
        },
    [{verify, verify_peer},
     {depth, 99},
     {cacerts, CACerts},
     {partial_chain, fun ?MODULE:partial_chain/1},
     {verify_fun, VerifyFun}].

check_cert(CACerts, Cert) ->
    % Taken from hackney, licensed under Apache 2 license.
    CertPKI = extract_public_key_info(Cert),
    lists:any(
      fun(CACert) ->
              extract_public_key_info(CACert) =:= CertPKI
      end, CACerts).

extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

lists_anymap(Fun, [H|T]) ->
    case Fun(H) of
        true -> {true, H};
        false -> lists_anymap(Fun, T)
    end;
lists_anymap(_Fun, []) ->
    false.

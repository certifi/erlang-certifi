-module('certifi').

%% API exports
-export([cacerts/0]).

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate that can then be used via the cacerts setting in ssl options
%% passed to the connect function.
cacerts() ->
    Pems = public_key:pem_decode(certifi_pemcerts:pemcerts()),
    [Der || {'Certificate', Der, _} <- Pems].

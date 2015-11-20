-module('certifi').

%% API exports
-export([cacerts/0]).
-export([weak_cacerts/0]).

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate that can then be used via the cacerts setting in ssl options
%% passed to the connect function.
cacerts() ->
    Pems = public_key:pem_decode(certifi_cacerts:pemcerts()),
    [Der || {'Certificate', Der, _} <- Pems].


%% @doc deprecated bundle. fix 1024-bit root issues
weak_cacerts() ->
    Pems = public_key:pem_decode(certifi_weak:pemcerts()),
    [Der || {'Certificate', Der, _} <- Pems].

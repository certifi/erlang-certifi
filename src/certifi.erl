-module(certifi).
-compile({parse_transform, certifi_pt}).

-export([cacerts/0, cacertfile/0]).

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate that can then be used via the cacerts setting in ssl options
%% passed to the connect function.
cacerts() ->
  %% Actual implementation replaced by parse transform.
  ok.

%% @doc CACertFile gives the path to the file with an X.509 certificate list
%% containing the Mozilla CA Certificate that can then be used via the
%% cacertfile setting in ssl options passed to the connect function.
cacertfile() ->
  filename:join(code:priv_dir(certifi), "cacerts.pem").

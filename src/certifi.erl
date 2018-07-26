-module(certifi).
-compile({parse_transform, ct_expand}).

-export([cacertfile/0,
         cacerts/0
        ]).


%% @doc CACertFile gives the path to the file with an X.509 certificate list
%% containing the Mozilla CA Certificate that can then be used via the
%% cacertfile setting in ssl options passed to the connect function.
cacertfile() ->
    ct_expand:term( certifi_db:cacertfile() ).

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate that can then be used via the cacerts setting in ssl options
%% passed to the connect function.
-spec cacerts() -> [binary(),...].
cacerts() ->
    ct_expand:term(
      lists:reverse(
        [Der || {ok,Bin} <- [file:read_file(certifi_db:cacertfile())],
                {'Certificate',Der,_} <- public_key:pem_decode(Bin)
        ]
       )
     ).

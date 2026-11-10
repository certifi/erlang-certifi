-module(certifi_pt).
-export([parse_transform/2]).

%% Embeds priv/cacerts.pem into certifi:cacerts/0. When CERTIFI_EMBED_EU is
%% set (environment variable or compile define), also embeds
%% priv/eu_cacerts.pem into certifi:eu_cacerts/0.
parse_transform(Forms, Opts) ->
  EmbedEu = embed_eu(Opts),
  [replace(Form, EmbedEu) || Form <- Forms].

replace({function, Ann, cacerts, 0, [_]}, _) ->
  embed(Ann, cacerts, "cacerts.pem");
replace({function, Ann, eu_cacerts, 0, [_]}, true) ->
  embed(Ann, eu_cacerts, "eu_cacerts.pem");
replace(Other, _) ->
  Other.

-spec embed_eu(Opts) -> boolean() when
	Opts :: [term()].
embed_eu(Opts) ->
  lists:member({d, 'CERTIFI_EMBED_EU'}, Opts)
    orelse lists:member({d, 'CERTIFI_EMBED_EU', true}, Opts)
    orelse lists:member(os:getenv("CERTIFI_EMBED_EU"), ["1", "true"]).

embed(Ann, Name, File) ->
  {ok, Binary} = file:read_file(cert_file(File)),
  Pems = public_key:pem_decode(Binary),
  Cacerts = [Der || {'Certificate', Der, _} <- Pems],
  Body = lists:foldl(fun(Cert, Acc) ->
    {cons, 0, cert_to_bin_ast(Cert), Acc}
  end, {nil, 0}, Cacerts),
  {function, Ann, Name, 0, [{clause, Ann, [], [], [Body]}]}.

-spec cert_file(File) -> Result when
	File :: string(),
	Result :: file:filename_all().
cert_file(File) ->
  AppDir = filename:dirname(
             filename:dirname(code:which(?MODULE))
            ),
  filename:join([AppDir, "priv", File]).

-spec cert_to_bin_ast(Cert) -> Result when
	Cert :: binary(),
	Result :: {bin, 0, [{bin_element, 0, {string, 0, list()}, default, default}]}.
cert_to_bin_ast(Cert) ->
  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Cert)}, default, default}]}.

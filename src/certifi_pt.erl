-module(certifi_pt).
-export([parse_transform/2]).

%% Embeds the selected bundles into certifi:cacerts/0 (mozilla) and
%% certifi:eu_cacerts/0 (eu). The selection comes from the CERTIFI_EMBED
%% environment variable, then the CERTIFI_EMBED compile define, and defaults
%% to mozilla only. Bundles that are not embedded are loaded at runtime.
parse_transform(Forms, Opts) ->
  Embed = embedded(Opts),
  [replace(Form, Embed) || Form <- Forms].

replace({function, Ann, cacerts, 0, [_]} = Form, Embed) ->
  case lists:member(mozilla, Embed) of
    true -> embed(Ann, cacerts, "cacerts.pem");
    false -> Form
  end;
replace({function, Ann, eu_cacerts, 0, [_]} = Form, Embed) ->
  case lists:member(eu, Embed) of
    true -> embed(Ann, eu_cacerts, "eu_cacerts.pem");
    false -> Form
  end;
replace(Other, _) ->
  Other.

-spec embedded(Opts) -> [mozilla | eu] when
	Opts :: [term()].
embedded(Opts) ->
  case os:getenv("CERTIFI_EMBED") of
    false ->
      case [V || {d, 'CERTIFI_EMBED', V} <- Opts] of
        [V | _] -> parse_embed(V);
        [] -> [mozilla]
      end;
    Env ->
      parse_embed(Env)
  end.

%% Accepts "mozilla,eu", "none", "" or a list of atoms such as [mozilla, eu].
parse_embed(none) -> [];
parse_embed(V) when is_atom(V) -> parse_embed([V]);
parse_embed(V) ->
  Names = case io_lib:printable_list(V) of
            true -> [list_to_atom(string:trim(N)) || N <- string:split(V, ",", all),
                                                      string:trim(N) =/= ""];
            false -> V
          end,
  case [N || N <- Names, not lists:member(N, [mozilla, eu, none])] of
    [] -> lists:usort(Names) -- [none];
    Bad -> erlang:error({invalid_certifi_embed, Bad,
                         "expected a combination of mozilla and eu, or none"})
  end.

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

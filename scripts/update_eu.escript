#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Build priv/eu_cacerts.pem from the EU List of Trusted Lists (LOTL).
%%
%% Every list is verified with xmlsec1 before it is used:
%%   - the LOTL against the keys pinned in scripts/eu_lotl_signers.pem;
%%   - each national list against the keys the verified LOTL publishes for it.
%%
%% The bundle keeps the certificates of granted CA/QC services marked
%% ForWebSiteAuthentication (QWAC issuers). When a national list cannot be
%% fetched or verified, that territory's previous entries are kept and a
%% warning is reported.
%%
%% Usage: escript scripts/update_eu.escript [--report FILE]
%% Requires: curl, xmlsec1.

-mode(compile).

-include_lib("public_key/include/public_key.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(LOTL_URL, "https://ec.europa.eu/tools/lotl/eu-lotl.xml").
-define(SIGNERS, "scripts/eu_lotl_signers.pem").
-define(BUNDLE, "priv/eu_cacerts.pem").
-define(TSL_MIME, "application/vnd.etsi.tsl+xml").
-define(LOTL_TYPE, "http://uri.etsi.org/TrstSvc/TrustedList/TSLType/EUlistofthelists").
-define(CA_QC, "http://uri.etsi.org/TrstSvc/Svctype/CA/QC").
-define(GRANTED, "http://uri.etsi.org/TrstSvc/TrustedList/Svcstatus/granted").
-define(WEB, "http://uri.etsi.org/TrstSvc/TrustedList/SvcInfoExt/ForWebSiteAuthentication").
-define(MAX_SIZE, "64M").

main(Args) ->
    Report = parse_args(Args),
    Tmp = make_tmp(),
    Code = try run(Tmp, Report) of
               ok -> 0
           catch
               throw:{fatal, Msg} ->
                   io:format(standard_error, "error: ~ts~n", [Msg]),
                   1
           end,
    _ = file:del_dir_r(Tmp),
    halt(Code).

parse_args([]) -> undefined;
parse_args(["--report", File]) -> File;
parse_args(_) ->
    io:format(standard_error, "usage: update_eu.escript [--report FILE]~n", []),
    halt(2).

run(Tmp, ReportFile) ->
    Tools = #{curl => executable("curl"),
              xmlsec => executable("xmlsec1"),
              lax => lax_key_search(executable("xmlsec1")),
              tmp => Tmp},

    Pinned = [Der || {Der, _} <- read_bundle_ders(?SIGNERS)],
    Pinned =/= [] orelse fatal("no pinned LOTL signer certificate in " ?SIGNERS),

    LotlFile = filename:join(Tmp, "lotl.xml"),
    ok = must(download(Tools, ?LOTL_URL, LotlFile, "=https"), "LOTL download"),
    LotlKey = must(verify(Tools, LotlFile, Pinned), "LOTL signature"),
    self_test(Tools, LotlFile),
    Lotl = must(parse(LotlFile), "LOTL parse"),
    ok = must(check_signature_shape(Lotl), "LOTL signature"),
    text(path(Lotl, ["SchemeInformation", "TSLType"])) =:= ?LOTL_TYPE
        orelse fatal("unexpected LOTL type"),

    Seq = text(path(Lotl, ["SchemeInformation", "TSLSequenceNumber"])),
    Issued = text(path(Lotl, ["SchemeInformation", "ListIssueDateTime"])),
    Pointers = pointers(Lotl),
    {Self, National} = lists:partition(
                         fun(#{territory := T, location := L}) ->
                                 T =:= "EU" andalso L =:= ?LOTL_URL
                         end, Pointers),

    Previous = read_bundle(?BUNDLE),
    Results = [collect(Tools, P) || P <- National],
    Entries = lists:append(
                [case R of
                     {ok, _T, Es, _W} -> Es;
                     {error, T, _} -> [E || E = #{territory := PT} <- Previous, PT =:= T]
                 end || R <- Results]),
    Bundle = dedupe(lists:sort(fun entry_order/2, Entries)),
    Bundle =/= [] orelse fatal("no certificate selected"),

    write_bundle(Bundle),
    SignersChanged = update_signers(Self, Pinned),

    Report = report(Seq, Issued, LotlKey, Results, Previous, Bundle, SignersChanged),
    io:put_chars(Report),
    case ReportFile of
        undefined -> ok;
        _ -> ok = file:write_file(ReportFile, Report)
    end.

%% Trusted list processing

pointers(Lotl) ->
    [#{territory => text(first(P, "SchemeTerritory")),
       location => text(child(P, "TSLLocation")),
       certs => [cert_der(C) || C <- descendants(P, "X509Certificate")]}
     || P <- paths(Lotl, ["SchemeInformation", "PointersToOtherTSL", "OtherTSLPointer"]),
        text(first(P, "MimeType")) =:= ?TSL_MIME].

collect(Tools, #{territory := T, location := Url, certs := Keys}) ->
    File = filename:join(maps:get(tmp, Tools), "tl-" ++ safe(T) ++ ".xml"),
    Steps = [fun() -> download(Tools, Url, File, "=https,http") end,
             fun() -> verify(Tools, File, Keys) end,
             fun() -> parse(File) end],
    case chain(Steps, undefined) of
        {ok, Doc} ->
            case check_list(Doc, T) of
                {ok, Warnings} ->
                    {Entries, Skipped} = services(Doc, T),
                    {ok, T, Entries, Warnings ++ skipped(Skipped)};
                {error, Why} ->
                    {error, T, Why}
            end;
        {error, Why} ->
            {error, T, Why}
    end.

chain([], Last) -> {ok, Last};
chain([F | Rest], _) ->
    case F() of
        ok -> chain(Rest, ok);
        {ok, V} -> chain(Rest, V);
        {error, _} = E -> E
    end.

check_list(Doc, Territory) ->
    Scheme = path(Doc, ["SchemeInformation"]),
    case {check_signature_shape(Doc), text(child(Scheme, "SchemeTerritory"))} of
        {{error, Why}, _} -> {error, Why};
        {ok, Territory} ->
            Next = text(path(Scheme, ["NextUpdate", "dateTime"])),
            Now = calendar:system_time_to_rfc3339(erlang:system_time(second), [{offset, "Z"}]),
            case Next =/= "" andalso Next < Now of
                true -> {ok, ["list expired on " ++ Next]};
                false -> {ok, []}
            end;
        {ok, Other} ->
            {error, "list declares territory " ++ Other}
    end.

%% Returns {Entries, Skipped}: certificates kept, and the ones OTP cannot
%% decode (they would be unusable as ssl trust anchors anyway).
services(Doc, Territory) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Found = [{usable_ca(Der, Now),
              #{territory => Territory, tsp => Tsp, service => name(child(Info, "ServiceName")),
                der => Der, sha256 => sha256(Der)}}
             || P <- paths(Doc, ["TrustServiceProviderList", "TrustServiceProvider"]),
                Tsp <- [name(path(P, ["TSPInformation", "TSPName"]))],
                Info <- paths(P, ["TSPServices", "TSPService", "ServiceInformation"]),
                text(child(Info, "ServiceTypeIdentifier")) =:= ?CA_QC,
                text(child(Info, "ServiceStatus")) =:= ?GRANTED,
                lists:member(?WEB, [text(U) || U <- descendants(child(Info, "ServiceInformationExtensions"), "URI")]),
                Id <- paths(Info, ["ServiceDigitalIdentity", "DigitalId"]),
                X <- children(Id, "X509Certificate"),
                Der <- [cert_der(X)]],
    {[E || {yes, E} <- Found], [E || {undecodable, E} <- Found]}.

skipped([]) -> [];
skipped(Es) ->
    [io_lib:format("skipped ~b certificate(s) public_key cannot decode: ~ts",
                   [length(Es), lists:join(", ", [H || #{sha256 := H} <- Es])])].

usable_ca(Der, Now) ->
    try public_key:pkix_decode_cert(Der, otp) of
        #'OTPCertificate'{tbsCertificate = Tbs} ->
            #'OTPTBSCertificate'{validity = #'Validity'{notAfter = NotAfter},
                                 extensions = Exts} = Tbs,
            case not_expired(NotAfter, Now) andalso is_ca(Exts) of
                true -> yes;
                false -> no
            end
    catch _:_ ->
        undecodable
    end.

not_expired(NotAfter, Now) ->
    pubkey_cert:time_str_2_gregorian_sec(NotAfter) > Now.

is_ca(asn1_NOVALUE) -> false;
is_ca(Exts) ->
    lists:any(fun(#'Extension'{extnID = ?'id-ce-basicConstraints',
                               extnValue = #'BasicConstraints'{cA = CA}}) -> CA =:= true;
                 (_) -> false
              end, Exts).

%% A trusted list must carry a single enveloped signature over the whole
%% document, so the content we read is the content that was signed.
check_signature_shape(Doc) ->
    case {children(Doc, "Signature"), descendants(Doc, "Signature")} of
        {[Sig], [Sig]} ->
            Refs = paths(Sig, ["SignedInfo", "Reference"]),
            Whole = [R || R <- Refs, attr(R, 'URI') =:= "",
                          lists:member("http://www.w3.org/2000/09/xmldsig#enveloped-signature",
                                       [attr(T, 'Algorithm') || T <- paths(R, ["Transforms", "Transform"])])],
            case Whole of
                [_] -> ok;
                _ -> {error, "signature does not cover the whole list"}
            end;
        _ ->
            {error, "expected exactly one signature element"}
    end.

%% Signature verification

verify(_Tools, _File, []) ->
    {error, "no signer certificate published"};
verify(Tools, File, Certs) ->
    case lists:dropwhile(fun(Der) -> xmlsec_verify(Tools, File, pubkey_pem(Der)) =/= ok end, Certs) of
        [Der | _] -> {ok, sha256(Der)};
        [] -> {error, "signature does not verify with any published key"}
    end.

%% Guard against an xmlsec1 build that would accept the certificate embedded
%% in the document instead of the key we pass.
self_test(Tools, File) ->
    #'RSAPrivateKey'{modulus = N, publicExponent = E} =
        public_key:generate_key({rsa, 2048, 65537}),
    Pem = public_key:pem_encode(
            [public_key:pem_entry_encode('SubjectPublicKeyInfo',
                                         #'RSAPublicKey'{modulus = N, publicExponent = E})]),
    case xmlsec_verify(Tools, File, Pem) of
        ok -> fatal("xmlsec1 accepted the LOTL with an unrelated key");
        error -> ok
    end.

xmlsec_verify(#{xmlsec := Xmlsec, lax := Lax, tmp := Tmp}, File, KeyPem) ->
    KeyFile = filename:join(Tmp, "key.pem"),
    ok = file:write_file(KeyFile, KeyPem),
    Args = ["--verify"] ++ Lax ++
        ["--enabled-reference-uris", "empty,same-doc",
         "--pubkey-pem", KeyFile, File],
    case cmd(Xmlsec, Args) of
        {0, _} -> ok;
        {_, _} -> error
    end.

%% xmlsec1 1.3 only uses keys passed on the command line with this flag;
%% 1.2 does so by default and does not know it.
lax_key_search(Xmlsec) ->
    {_, Help} = cmd(Xmlsec, ["--help-verify"]),
    case binary:match(Help, <<"--lax-key-search">>) of
        nomatch -> [];
        _ -> ["--lax-key-search"]
    end.

%% Signer certificates do not always decode with public_key (non-conforming
%% name encodings), so take the SubjectPublicKeyInfo straight from the DER.
pubkey_pem(Der) ->
    {16#30, Cert, _} = der_tlv(Der),
    {16#30, Tbs, _} = der_tlv(Cert),
    Fields = der_seq(Tbs),
    Spki = case Fields of
               [{16#A0, _} | Rest] -> lists:nth(6, Rest);
               _ -> lists:nth(6, Fields)
           end,
    {16#30, Raw} = Spki,
    public_key:pem_encode([{'SubjectPublicKeyInfo', Raw, not_encrypted}]).

%% Returns {Tag, Content, Rest}; Raw TLV is kept for sequence members.
der_tlv(<<Tag, 0:1, Len:7, Content:Len/binary, Rest/binary>>) ->
    {Tag, Content, Rest};
der_tlv(<<Tag, 1:1, N:7, Len:N/unit:8, Content:Len/binary, Rest/binary>>) ->
    {Tag, Content, Rest}.

der_seq(<<>>) -> [];
der_seq(Bin) ->
    {Tag, _, Rest} = der_tlv(Bin),
    Size = byte_size(Bin) - byte_size(Rest),
    <<Raw:Size/binary, _/binary>> = Bin,
    [{Tag, Raw} | der_seq(Rest)].

%% Download

download(#{curl := Curl}, Url, File, Proto) ->
    Args = ["--fail", "--silent", "--show-error", "--location",
            "--proto", Proto, "--proto-redir", Proto,
            "--max-time", "120", "--retry", "2", "--max-filesize", ?MAX_SIZE,
            "--output", File, Url],
    case cmd(Curl, Args) of
        {0, _} -> ok;
        {_, Out} -> {error, "download failed: " ++ string:trim(binary_to_list(Out))}
    end.

%% XML

parse(File) ->
    {ok, Bin} = file:read_file(File),
    case binary:match(Bin, <<"<!DOCTYPE">>) of
        nomatch ->
            try xmerl_scan:file(File, [{namespace_conformant, true},
                                       {validation, off},
                                       {fetch_fun, fun(_, _) -> {error, no_fetch} end},
                                       {quiet, true}]) of
                {Doc, _Rest} -> {ok, Doc}
            catch _:_ ->
                {error, "XML parse error"}
            end;
        _ ->
            {error, "DOCTYPE is not allowed"}
    end.

local(#xmlElement{name = Name}) ->
    case string:split(atom_to_list(Name), ":", trailing) of
        [_, Local] -> Local;
        [Local] -> Local
    end.

children(undefined, _) -> [];
children(#xmlElement{content = Content}, Name) ->
    [C || C = #xmlElement{} <- Content, local(C) =:= Name].

child(E, Name) ->
    case children(E, Name) of
        [C | _] -> C;
        [] -> undefined
    end.

path(E, []) -> E;
path(E, [Name | Rest]) -> path(child(E, Name), Rest).

paths(E, [Name]) -> children(E, Name);
paths(E, [Name | Rest]) -> lists:append([paths(C, Rest) || C <- children(E, Name)]).

descendants(undefined, _) -> [];
descendants(#xmlElement{content = Content}, Name) ->
    lists:append([[C || local(C) =:= Name] ++ descendants(C, Name)
                  || C = #xmlElement{} <- Content]).

first(E, Name) ->
    case descendants(E, Name) of
        [C | _] -> C;
        [] -> undefined
    end.

text(undefined) -> "";
text(#xmlElement{content = Content}) ->
    string:trim(lists:flatten([V || #xmlText{value = V} <- Content])).

attr(#xmlElement{attributes = Attrs}, Name) ->
    case lists:keyfind(Name, #xmlAttribute.name, Attrs) of
        #xmlAttribute{value = V} -> V;
        false -> undefined
    end.

%% English name if present, otherwise the first one.
name(E) ->
    Names = children(E, "Name"),
    Pick = case [N || N <- Names, attr(N, 'xml:lang') =:= "en"] of
               [En | _] -> En;
               [] when Names =/= [] -> hd(Names);
               [] -> undefined
           end,
    clean(text(Pick)).

clean(S) ->
    string:trim([case C < 32 orelse C =:= 127 of true -> $\s; false -> C end || C <- S]).

cert_der(E) ->
    base64:decode([C || C <- text(E), not lists:member(C, " \t\r\n")]).

%% Bundle file

entry_order(A, B) ->
    key(A) =< key(B).

key(#{territory := T, tsp := P, service := S, sha256 := H}) -> {T, P, S, H}.

dedupe(Entries) ->
    {Kept, _} = lists:foldl(
                  fun(E = #{sha256 := H}, {Acc, Seen}) ->
                          case maps:is_key(H, Seen) of
                              true -> {Acc, Seen};
                              false -> {[E | Acc], Seen#{H => true}}
                          end
                  end, {[], #{}}, Entries),
    lists:reverse(Kept).

write_bundle(Entries) ->
    Header =
        "##\n"
        "## CA certificates for qualified website authentication (QWAC)\n"
        "## from the EU trusted lists.\n"
        "##\n"
        "## Source: " ?LOTL_URL "\n"
        "##\n"
        "## Each certificate is the digital identity of a granted CA/QC service\n"
        "## marked ForWebSiteAuthentication. Many are issuing CAs, not roots.\n"
        "##\n"
        "## Generated by scripts/update_eu.escript. Do not edit by hand.\n"
        "##\n\n",
    Body = [[ "# Territory: ", T, "\n",
              "# TSP: ", P, "\n",
              "# Service: ", S, "\n",
              "# SHA256: ", H, "\n",
              public_key:pem_encode([{'Certificate', Der, not_encrypted}])]
            || #{territory := T, tsp := P, service := S, sha256 := H, der := Der} <- Entries],
    write_atomic(?BUNDLE, unicode:characters_to_binary([Header, Body])).

read_bundle(File) ->
    [entry(Der, Meta) || {Der, Meta} <- read_bundle_ders(File)].

entry(Der, Meta) ->
    #{territory => maps:get("Territory", Meta, ""), tsp => maps:get("TSP", Meta, ""),
      service => maps:get("Service", Meta, ""), der => Der, sha256 => sha256(Der)}.

%% Returns [{Der, #{Header => Value}}] using the "# Key: Value" lines that
%% precede each certificate.
read_bundle_ders(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Lines = string:split(unicode:characters_to_list(Bin), "\n", all),
            read_lines(Lines, #{}, []);
        {error, enoent} ->
            []
    end.

read_lines([], _, Acc) ->
    lists:reverse(Acc);
read_lines(["-----BEGIN CERTIFICATE-----" | Rest], Meta, Acc) ->
    {B64, ["-----END CERTIFICATE-----" | Rest1]} =
        lists:splitwith(fun(L) -> L =/= "-----END CERTIFICATE-----" end, Rest),
    read_lines(Rest1, #{}, [{base64:decode(lists:append(B64)), Meta} | Acc]);
read_lines(["# " ++ Line | Rest], Meta, Acc) ->
    case string:split(Line, ": ") of
        [K, V] -> read_lines(Rest, Meta#{K => V}, Acc);
        _ -> read_lines(Rest, Meta, Acc)
    end;
read_lines([_ | Rest], Meta, Acc) ->
    read_lines(Rest, Meta, Acc).

%% The verified LOTL announces the keys allowed to sign its next versions.
%% Keep the pinned set in sync so key rotations do not break updates.
update_signers([#{certs := Certs} | _], Pinned) when Certs =/= [] ->
    Sorted = lists:usort(Certs),
    case Sorted =:= lists:usort(Pinned) of
        true ->
            false;
        false ->
            Pem = [["# SHA256: ", sha256(D), "\n",
                    public_key:pem_encode([{'Certificate', D, not_encrypted}])] || D <- Sorted],
            write_atomic(?SIGNERS, iolist_to_binary(Pem)),
            true
    end;
update_signers(_, _) ->
    fatal("the LOTL does not publish its own signer certificates").

write_atomic(File, Data) ->
    Tmp = File ++ ".tmp",
    ok = file:write_file(Tmp, Data),
    ok = file:change_mode(Tmp, 8#644),
    ok = file:rename(Tmp, File).

%% Report

report(Seq, Issued, LotlKey, Results, Previous, Bundle, SignersChanged) ->
    Old = maps:from_list([{H, E} || E = #{sha256 := H} <- Previous]),
    New = maps:from_list([{H, E} || E = #{sha256 := H} <- Bundle]),
    Added = [E || {H, E} <- lists:sort(maps:to_list(New)), not maps:is_key(H, Old)],
    Removed = [E || {H, E} <- lists:sort(maps:to_list(Old)), not maps:is_key(H, New)],
    Count = fun(T) -> length([x || #{territory := BT} <- Bundle, BT =:= T]) end,
    Rows = [case R of
                {ok, T, _, []} -> io_lib:format("| ~s | ~b | ok |~n", [T, Count(T)]);
                {ok, T, _, W} -> io_lib:format("| ~s | ~b | ~ts |~n", [T, Count(T), lists:join("; ", W)]);
                {error, T, Why} -> io_lib:format("| ~s | ~b | kept previous entries: ~ts |~n", [T, Count(T), Why])
            end || R <- Results],
    Line = fun(#{territory := T, tsp := P, service := S, sha256 := H}) ->
                   io_lib:format("- ~s, ~ts, ~ts (`~s`)~n", [T, P, S, H])
           end,
    unicode:characters_to_binary(
      [io_lib:format("## EU trusted lists~n~n"
                     "LOTL sequence ~s issued ~s, verified with key `~s`.~n~n"
                     "~b certificates (~b added, ~b removed).~n~n",
                     [Seq, Issued, LotlKey, length(Bundle), length(Added), length(Removed)]),
       case SignersChanged of
           true -> "The LOTL signer keys changed; `" ?SIGNERS "` was updated.\n\n";
           false -> ""
       end,
       "| Territory | Certificates | Status |\n|---|---|---|\n", Rows, "\n",
       case Added of [] -> ""; _ -> ["### Added\n\n", [Line(E) || E <- Added], "\n"] end,
       case Removed of [] -> ""; _ -> ["### Removed\n\n", [Line(E) || E <- Removed], "\n"] end]).

%% Helpers

sha256(Der) ->
    string:lowercase(binary_to_list(binary:encode_hex(crypto:hash(sha256, Der)))).

safe(S) ->
    [C || C <- S, (C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z)].

executable(Name) ->
    case os:find_executable(Name) of
        false -> fatal(Name ++ " not found in PATH");
        Path -> Path
    end.

cmd(Exe, Args) ->
    Port = open_port({spawn_executable, Exe},
                     [{args, Args}, exit_status, stderr_to_stdout, binary, hide]),
    cmd_loop(Port, []).

cmd_loop(Port, Acc) ->
    receive
        {Port, {data, D}} -> cmd_loop(Port, [Acc, D]);
        {Port, {exit_status, S}} -> {S, iolist_to_binary(Acc)}
    end.

make_tmp() ->
    Base = case os:getenv("TMPDIR") of
               false -> "/tmp";
               "" -> "/tmp";
               Dir -> Dir
           end,
    Tmp = filename:join(Base, "certifi-eu-" ++ integer_to_list(erlang:unique_integer([positive]))
                        ++ "-" ++ os:getpid()),
    ok = file:make_dir(Tmp),
    Tmp.

must(ok, _) -> ok;
must({ok, V}, _) -> V;
must({error, Why}, What) -> fatal(What ++ ": " ++ Why).

fatal(Msg) ->
    throw({fatal, Msg}).

#!/usr/bin/env escript
%% -*- erlang -*-
-module(compile).

-define(CERTS_MOD, "certifi_cacerts").


build_module(Mod, Certs) ->
    Pems = public_key:pem_decode(Certs),
    Cacerts = [Der || {'Certificate', Der, _} <- Pems],
    Src = ["-module(", Mod, ").\n",
           "-compile([compressed, no_line_info]).\n",
           "-export([ders/0]).\n\n",
           "ders() ->", io_lib:format("~n    ~p.~n", [Cacerts])],
    ok = file:write_file(Mod ++ ".erl", Src),
    ok.

main(_) ->
  {ok, Cacerts} = file:read_file("../certs_spec/cacerts.pem"),
  ok = build_module(?CERTS_MOD, Cacerts),
  ok.

-module(certifi).
%% Causes the parse transformation function Module:parse_transform/2
%% to be applied to the parsed code before the code is checked for errors.
-compile({parse_transform, certifi_pt}).

-export([cacertfile/0,
         cacerts/0]).

%% @doc CACertFile gives the path to the file with an X.509 certificate list
%% containing the Mozilla CA Certificate.
%%
%% The file is used via the `cacertfile' setting in {@link //ssl/ssl} options,
%% passed to the `ssl:connect/2,3,4' function.
-spec cacertfile() -> Result when
	Result :: file:filename_all().
cacertfile() ->
  PrivDir = case code:priv_dir(certifi) of
    {error, _} ->
      %% try to get relative priv dir. useful for tests.
      AppDir = filename:dirname(
                 filename:dirname(code:which(?MODULE))
                ),
      filename:join(AppDir, "priv");
    Dir -> Dir
  end,
  filename:join(PrivDir, "cacerts.pem").

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate.
%%
%% The certificate list is used via the `cacerts' setting in {@link //ssl/ssl}
%% options, passed to the `connect/2,3,4' function.
-spec cacerts() -> [binary(),...].
cacerts() ->
    ok.

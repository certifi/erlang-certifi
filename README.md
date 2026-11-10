# [certifi](https://github.com/certifi/erlang-certifi)

[![CI](https://github.com/certifi/erlang-certifi/actions/workflows/erlang.yml/badge.svg)](https://github.com/certifi/erlang-certifi/actions/workflows/erlang.yml)
[![Hex](https://img.shields.io/hexpm/v/certifi.svg)](https://hex.pm/packages/certifi)
[![HexDocs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/certifi)

This Erlang library contains a CA bundle that you can reference in your Erlang
application. This is useful for systems that do not have CA bundles that
Erlang can find itself, or where a uniform set of CAs is valuable.

This an Erlang specific port of [certifi](https://certifi.io/). The CA bundle
is derived from Mozilla's canonical set.

The bundled 118 certificates were verified on 2026-10-23 against the
[Mozilla CA bundle published by curl](https://curl.se/docs/caextract.html),
dated 2026-10-20. The distrusted emSign C1, C3, and G3 roots are excluded.

## Usage

```erlang
CaCerts = certifi:cacerts(),
SslOptions = [{verify, verify_peer},
              {depth, 99},
              {cacerts, CaCerts}],
ssl:connect( "example.com", 443, SslOptions ).
```


You can also retrieve the path to the file and load it by yourself if needed:

```erlang
Path = certifi:cacertfile().
```

## EU trusted lists

certifi also ships an opt-in bundle with the CA certificates EU member states
list as qualified to issue website authentication certificates (QWACs):

```erlang
EuCerts = certifi:eu_cacerts(),
EuPath = certifi:eu_cacertfile().
```

Many of these are issuing CAs rather than roots, so you need a `partial_chain`
option to use them with `ssl`. See
[EU trusted lists](guides/eu-trusted-lists.md) for usage and how the bundle
is built.

## Choose which bundles are compiled in

By default only the Mozilla bundle is embedded in the module. Select the
bundles to embed with `CERTIFI_EMBED` (`mozilla`, `eu`, `mozilla,eu` or
`none`). Bundles that are not embedded are read from `priv` on first use, so
both functions always work.

```erlang
%% rebar.config of your application
{overrides, [
    {add, certifi, [{erl_opts, [{d, 'CERTIFI_EMBED', [mozilla, eu]}]}]}
]}.
```

```shell
CERTIFI_EMBED=eu rebar3 compile
```

See [Choose which bundles are compiled in](guides/eu-trusted-lists.md#choose-which-bundles-are-compiled-in)
for details.

## Changelog

See [CHANGELOG.md](CHANGELOG.md).

## Development

Update the CA bundle from curl's Mozilla extraction (requires curl and OpenSSL):

```shell
make update
rebar3 clean
rebar3 eunit
```

The update verifies the published SHA-256 checksum and parses the certificates
before replacing `priv/cacerts.pem`. Review changes to the trust roots and update
the test expectations before committing. Clean before rebuilding because the
certificates are embedded into the module at compile time.

Update the EU bundle from the EU trusted lists (requires curl, xmlsec1 and
OTP 24 or later):

```shell
make update-eu
rebar3 clean
rebar3 eunit
```

Every list is signature checked before use. See
[EU trusted lists](guides/eu-trusted-lists.md#update-the-bundle) for details.

A scheduled workflow runs both updates every week and opens a pull request
when a bundle changed.

Testing:

```shell
rebar3 eunit
```

Documentation:

```shell
rebar3 ex_doc
```

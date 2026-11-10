# EU trusted lists

certifi ships a second, opt-in bundle with the CA certificates that EU member
states list as qualified to issue website authentication certificates (QWACs)
under the eIDAS regulation. Use it when you must accept servers presenting a
QWAC whose issuer is not in the Mozilla bundle, for example when talking to
public administrations, banks under PSD2, or other eIDAS services. For general
HTTPS, keep using `certifi:cacerts/0`.

## What is in the bundle

The bundle is built from the
[EU List of Trusted Lists](https://eidas.ec.europa.eu/efda/tl-browser/)
(LOTL), published by the European Commission. It keeps the certificates of
every trust service that is:

- of type `CA/QC` (issues qualified certificates),
- in status `granted`,
- marked `ForWebSiteAuthentication`,
- a CA certificate that has not expired and that `public_key` can decode.

Each entry in `priv/eu_cacerts.pem` names its territory, trust service
provider and service, so you can review what you trust.

Many of these certificates are issuing (intermediate) CAs, not self-signed
roots. OTP `ssl` only accepts them as trust anchors through `partial_chain`,
as shown below.

## Connect to a server with a QWAC

```erlang
EuCerts = certifi:eu_cacerts(),
PartialChain = fun(Chain) ->
    case [Cert || Cert <- Chain, lists:member(Cert, EuCerts)] of
        [Trusted | _] -> {trusted_ca, Trusted};
        [] -> unknown_ca
    end
end,
{ok, Socket} = ssl:connect("example.eu", 443,
                           [{verify, verify_peer},
                            {cacerts, EuCerts},
                            {partial_chain, PartialChain},
                            {depth, 99}]).
```

## Accept both Mozilla and EU issuers

```erlang
EuCerts = certifi:eu_cacerts(),
CaCerts = certifi:cacerts() ++ EuCerts,
PartialChain = fun(Chain) ->
    case [Cert || Cert <- Chain, lists:member(Cert, EuCerts)] of
        [Trusted | _] -> {trusted_ca, Trusted};
        [] -> unknown_ca
    end
end,
SslOptions = [{verify, verify_peer},
              {cacerts, CaCerts},
              {partial_chain, PartialChain},
              {depth, 99}].
```

## Use the PEM file

```erlang
Path = certifi:eu_cacertfile(),
SslOptions = [{verify, verify_peer}, {cacertfile, Path}].
```

`partial_chain` still applies when the issuer in the file is not a root.

## Choose which bundles are compiled in

Both bundles are always available: `certifi:cacerts/0` (Mozilla) and
`certifi:eu_cacerts/0` (EU) work whatever you choose. The choice only decides
which ones are embedded in `certifi.beam` at compile time. A bundle that is
not embedded is read from its PEM file in `priv` on first call and cached in
`persistent_term`.

| `CERTIFI_EMBED`  | Embedded         | `certifi.beam` |
|------------------|------------------|----------------|
| `mozilla`        | Mozilla only (default) | 225 KB   |
| `eu`             | EU only          | 375 KB         |
| `mozilla,eu`     | Both             | 595 KB         |
| `none`           | Neither          | 3 KB           |

Embed the bundles you use on every connection. Leave out the ones you rarely
use, or all of them if you prefer a smaller module and a file read on first
use.

### In your rebar.config

Add an override for the certifi dependency:

```erlang
{deps, [certifi]}.

{overrides, [
    {add, certifi, [{erl_opts, [{d, 'CERTIFI_EMBED', [mozilla, eu]}]}]}
]}.
```

The value is a list of `mozilla` and `eu`, or `none`. rebar3 recompiles
certifi when you change it.

### With an environment variable

```shell
CERTIFI_EMBED=mozilla,eu rebar3 compile
```

rebar3 does not track environment variables, so rebuild certifi when you
change it:

```shell
rebar3 clean --apps=certifi
CERTIFI_EMBED=eu rebar3 compile
```

With Mix:

```shell
CERTIFI_EMBED=mozilla,eu mix deps.compile certifi --force
```

### Notes

- The environment variable wins over the `rebar.config` setting.
- An unknown name, such as `mozila`, stops the compilation with an
  `invalid_certifi_embed` error.
- `certifi:cacertfile/0` and `certifi:eu_cacertfile/0` return the PEM paths
  in every mode.
- Releases must include certifi's `priv` directory when a bundle is not
  embedded. rebar3 and Mix releases do this by default.

## Update the bundle

You need curl, xmlsec1 and OTP 24 or later.

```shell
make update-eu
rebar3 clean
rebar3 eunit
```

The update:

1. downloads the LOTL and verifies its XML signature against the keys pinned
   in `scripts/eu_lotl_signers.pem`;
2. downloads each national list and verifies it against the keys the LOTL
   publishes for that territory;
3. rewrites `priv/eu_cacerts.pem` and prints a report of added and removed
   certificates.

If a national list cannot be downloaded or verified, the previous entries for
that territory are kept and the report says so. When the LOTL announces new
signer keys, `scripts/eu_lotl_signers.pem` is updated from the verified list.
If the LOTL is signed with a key that is not pinned, the update stops.

A scheduled GitHub workflow runs the update every week and opens a pull
request when something changed.

## Notes

- The EU lists say who is qualified under eIDAS. They are not reviewed under
  the Mozilla root program, so do not merge this bundle into a general
  purpose trust store.
- To check the pinned LOTL signer keys, compare
  `scripts/eu_lotl_signers.pem` with the certificates published in the
  Official Journal of the EU.
- On OTP 21 and 22, `ssl` cannot decode the few certificates signed with
  RSA-PSS. It ignores them with an "Ignoring a CA cert" warning and uses the
  rest of the bundle.
- Revocation is not checked. Use OTP `ssl` CRL options if you need it.

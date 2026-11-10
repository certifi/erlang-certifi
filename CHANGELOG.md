# Changelog

## 2.19.0 (2026-11-10)

- New opt-in EU bundle: `certifi:eu_cacerts/0` and `certifi:eu_cacertfile/0`
  return the CA certificates EU member states list as qualified to issue
  website authentication certificates (QWACs). Use it when you need to accept
  servers whose issuer is in the EU trusted lists but not in the Mozilla
  bundle. The default `cacerts/0` is unchanged.
- Choose which bundles are embedded in the module at compile time with
  `CERTIFI_EMBED` (`mozilla`, `eu`, `mozilla,eu` or `none`), as an
  environment variable or in your rebar.config. Bundles that are not embedded
  are read from disk on first use. The default embeds the Mozilla bundle only,
  as before.
- The EU bundle is built only from trusted lists whose signatures verify, and a
  weekly job now proposes updates to both bundles as pull requests.
- certifi now declares `public_key` as a runtime dependency.

## 2.18.0 (2026-10-23)

- Verify the 118 bundled roots match curl's Mozilla bundle dated 2026-10-20,
  including removal of the distrusted emSign C1, C3, and G3 roots.
- Download updates from curl's Mozilla CA extraction, verify its SHA-256 checksum,
  and validate the PEM before replacing the bundle.
- Test that the embedded roots match the PEM file and exclude distrusted emSign roots.

## 2.17.0 (2026-06-05)

- Update CA bundle to the Mozilla set as of May 2026 (119 certificates).
  Distrusted roots removed upstream (including the Entrust roots) are dropped.

## 2.16.0 (2025-12-29)

- Update CA bundle.
- Support OTP 28 in CI and bump actions/checkout.
- Remove deprecation warning.

## 2.15.0 (2025-05-25)

- Update CA bundle.

# Changelog

## Unreleased

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

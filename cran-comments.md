# writeAlizer 1.7.4

## R CMD check results

Passed `R CMD check` on:

*   R-release-windows-x64
*   R-devel-linux-x64-ubuntu
*   R-release-linux-x64-ubuntu
*   R-oldrel-1-linux-x64-ubuntu
*   R-release-macos-arm64

No ERRORs, WARNINGs, or NOTEs.

## Reverse dependency checks

No reverse dependencies for this package.

## Changes in this version

### Bug fixes
- Preserved text IDs and missing numeric values during import, with clearer input validation.
- Fixed local file downloads and improved cache validation, model loading, and prediction checks.

### Documentation
- Improved the getting-started guide, score interpretation, setup instructions, and website navigation.
- Updated the Matta, Keller-Margulis, and Mercer (2025) citation with published article details.

### Tests and maintenance
- Improved test coverage and isolation, and made examples restore settings after use.
- Updated minimum requirements to R 4.0.0 and testthat 3.2.0.

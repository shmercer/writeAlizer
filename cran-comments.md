# writeAlizer 1.7.3

## R CMD check results

Passed `R CMD check` on:

*   R-release-windows-x64
*   R-devel-linux-x64-ubuntu
*   R-release-linux-x64-ubuntu
*   R-oldrel-linux-x64-ubuntu
*   R-release-macos-x64

No ERRORs or WARNINGs.

## Reverse dependency checks

No reverse dependencies for this package.

## Changes in this version

### CRAN policy
- Wrapped one example in `\\dontrun{}` instead of `\\donttest{}` to prevent blackswan CRAN check 
  notes when running tests wrapped in `\\donttest{}`. The example downloads files from an external API


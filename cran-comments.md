# writeAlizer 1.7.2

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

* new features
  * Added `keep_stem_before_txt()` function to process Coh-Metrix and GAMET filenames that appear as paths
  * integrated `keep_stem_before_txt()` into `import_coh()` `import_gamet()` and `import_merge_gamet_rb()` functions

* improved documentation
  * Added new vignette that details scoring model development
  * Added a `pkgdown()` site to host documentation on GitHub


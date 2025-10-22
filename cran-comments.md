# writeAlizer 1.7.1

### Resubmission summary

This is a resubmission of **writeAlizer 1.7.0**, addressing the Fedora/Debian CRAN check error related to example execution:

* The `predict_quality()` examples previously attempted to download model artifacts when CRAN executed `--run-donttest`.  
* These examples now explicitly set `options(writeAlizer.offline = TRUE)` to disable downloads during checks.  
* Network-dependent examples are conditionally skipped if offline mode is active.  
* Local and WinBuilder checks confirm that all examples now run deterministically and produce identical outputs across platforms.  

No functional or API-level changes were made to the package; this update solely improves CRAN compliance and test stability.

### Changes since last submission

* Fixed CRAN check error on Fedora/Debian related to `predict_quality()` examples:
  - Added a one-line offline guard at the top of examples:
    ```r
    old_offline <- getOption("writeAlizer.offline")
    options(writeAlizer.offline = TRUE)
    on.exit(options(writeAlizer.offline = old_offline), add = TRUE)
    ```
  - Prevents network downloads when CRAN runs with `--run-donttest`.
  - Keeps the offline “example” model demo runnable for users.
* Wrapped the extended “\donttest{}” demos in a runtime guard so they are skipped when offline mode is active on CRAN.
* No runtime changes to package functions.
* Documentation rebuilt with `devtools::document()`.
* Confirmed that cache and artifact directories are not written to `~/.cache` during examples or tests.

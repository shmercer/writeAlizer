## Resubmission: writeAlizer 1.6.3

This submission addresses CRAN review feedback.

### DESCRIPTION
- Explained acronyms at first use.
- Quoted software and package names with single quotes and correct case (e.g., 'ReaderBench', 'Coh-Metrix', 'GAMET').
- Added in-text method references with CRAN’s autolink format

### Documentation
- Added `@return` to all exported functions flagged by CRAN:
  - `import_coh()`: returns a data frame with `ID` + selected 'Coh-Metrix' features.
  - `import_rb()`: returns a data frame with `ID` + selected 'ReaderBench' features.
  - `import_gamet()`: returns a data frame with `ID` + 'GAMET' error counts/categories.
  - `import_merge_gamet_rb()`: returns a joined data frame on `ID` with aligned feature columns.
- Replaced `\\dontrun{}` with `\\donttest{}` where appropriate; unwrapped examples that run in <5 seconds.
- `predict_quality()` example is now fast & offline via a seeded tiny `'example'` model in `tempdir()`, with cleanup.

### File I/O policy
- No functions write to the user’s home directory by default.
- Examples, tests, and any temporary work use `tempdir()`.
- Package cache uses `tools::R_user_dir("writeAlizer", "cache")`.
- Added helpers `wa_cache_dir()` and `wa_cache_clear(ask=)`; tests ensure no temp detritus.

### Package installation policy
- Removed installation behavior from any runtime code.
- New `model_deps()` reports optional model dependencies and prints a copy-paste `install.packages()` command; it does not install.
- Deprecated `install_model_deps()`; it now only warns and delegates.

### Checks
- Local `R CMD check --as-cran` on Windows 11 / R 4.5.1: 0 errors, 0 warnings, 0 notes.
- Tests use self-cleaning fixtures and do not leave files in `tempdir()`.

Thank you for reviewing.

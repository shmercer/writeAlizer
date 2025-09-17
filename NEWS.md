# writeAlizer 1.6.3 (2025-09-17)

## CRAN policy & documentation improvements
- DESCRIPTION: acronyms expanded (e.g., CBM), software/package names single-quoted, and method references added with CRAN auto-link format (DOIs).
- Documentation: `@return` added to `import_coh()`, `import_rb()`, `import_gamet()`, `import_merge_gamet_rb()`.
- Examples: `\\dontrun{}` replaced with `\\donttest{}` where needed; fast examples unwrapped; `predict_quality()` uses a tiny offline `'example'` model seeded in `tempdir()` with cleanup.
- File I/O: no writes to home by default; examples/tests use `tempdir()`; caching standardized via `tools::R_user_dir("writeAlizer","cache")`.
- New helpers: `wa_cache_dir()` and `wa_cache_clear(ask=)` for cache discovery/cleanup.
- Optional deps: new `model_deps()` reports (does not install) optional packages and prints an install hint; `install_model_deps()` deprecated.
- Tests: self-cleaning with `withr` fixtures; no temp detritus; removed deprecated global teardown.

## Bug fixes & maintenance
- Stabilized example/test behavior when offline by mocking artifacts via `writeAlizer.mock_dir`.
- Suppressed noisy pre-processing variance warnings in integration tests.


# writeAlizer 1.6.2 (2025-09-14)

- Simplified `install_model_deps()`:
  - Removed unused `model` argument.
  - Standardized on the `options(writeAlizer.require_pkgs_for_fits = function() ...)` hook; dropped internal helper.
  - Single `Suggests` discovery path via `utils::packageDescription(..., fields = "Suggests")`.
  - `dry_run = TRUE` now returns tokens visibly (character vector); version qualifiers are stripped only for checks/installs.
- Tests updated

# writeAlizer 1.6.1 (2025-09-12)

- Test coverage raised to ~82% overall.
- `artifact_registry`: prefer CSV (`inst/metadata/artifacts.csv`) and remove legacy in-code fallback table; tighter input validation.
- Quieter tests by silencing download/file messages and using local cache paths during tests.
- More robust `install_model_deps()`:
  - Clear dry-run behavior (returns Suggests tokens or helper-provided list).
  - Strips version qualifiers before installation; installs only truly missing packages.
  - New helper hook via `options(writeAlizer.require_pkgs_for_fits = function(model, install, return_pkgs) {...})`.
- Documentation: examples added for `install_model_deps()`; tidied roxygen.
- Stabilized artifact fetching in tests (`.wa_ensure_file`) with explicit cache dirs and checksum paths.
- Eliminated intermittent warnings from `download.file()` and file URLs during tests.
- Added/expanded tests for registry filtering, checksum guards, loader utilities, and package-requirement collection.
- Removed unused/legacy code paths exercised only by fallback artifact table.

# writeAlizer 1.6.0 (2025-09-12)

- Replaced the ReaderBench mod3 models with their v2 counterparts (e.g., `rb_mod3all` now calls the `rb_mod3all_v2` models).
- Optimized model artifact downloads and added SHA-256 checksum verification (auto re-download on mismatch).
- `predict_quality()` now returns per-submodel scores **and** the overall mean column `pred_<model>_mean` (where applicable).
- `import_rb()` now keeps ReaderBench features **by name** using the packaged `sample_rb.csv` header (first 404), dropping 405+. It falls back to the legacy positional selection if the header is unavailable, reducing brittleness to column reordering.
- Added `install_model_deps()` helper to install Suggests for a given model.
- CI: wired up GitHub Actions R-CMD-check (Windows/macOS/Linux).

# writeAlizer 1.5.0 (2024-02-09)

- Added new genre-specific scoring models for ReaderBench (`rb_mod3narr_v2`, `rb_mod3exp_v2`, `rb_mod3per_v2`, `rb_mod3all_v2`)
  and Coh-Metrix (`coh_mod3narr`, `coh_mod3exp`, `coh_mod3per`, `coh_mod3all`).

# writeAlizer 1.4.0 (2024-01-30)

- Added new ReaderBench genre-specific scoring models (`rb_mod3narr`, `rb_mod3exp`, `rb_mod3per`, `rb_mod3all`).

# writeAlizer 1.3.0 (2021-11-10)

- Replaced RB/Coh model 2s with trimmed versions of model 1s with improved data pre-processing.

# writeAlizer 1.2.0 (2020-11-11)

- Moved model artifacts to OSF hosting.
- Added RB/Coh model 2s (trimmed versions of model 1 ensembles).

# writeAlizer 1.1.0 (2020-08-03)

- Model artifacts now download during first `predict_quality()` call.

# writeAlizer 1.0.4 (2020-07-29)

- Fixed ReaderBench file import.

# writeAlizer 1.0.3 (2020-05-29)

- Now accepts ReaderBench output files as `.csv`.

# writeAlizer 1.0.2 (2020-05-27)

- `predict_quality()` now also returns an `ID` variable.

# writeAlizer 1.0.1 (2020-05-14)

- Added model fit objects to package and test data.

# writeAlizer 1.0.0 (2020-05-01)

- Initial version.

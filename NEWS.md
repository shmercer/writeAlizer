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

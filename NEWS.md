writeAlizer v1.6.0 (Release date: 2025-9-10)
==============
Changes: 
* Replaced the RB mod3 models with their v2 counterparts (e.g., rb_mod3all now calls the rb_mod3all_v2 models).
* Optimized code for the downloading of model objects, adding checksum verification.
* predict_quality now outputs scores for submodels as well as the overall mean across submodels
* `import_rb()` now keeps ReaderBench features **by name** using the packaged `sample_rb.csv` header (first 404), dropping 405+. Falls back to the legacy positional selection if the header is unavailable. This reduces brittleness to column reordering in ReaderBench outputs.

writeAlizer v1.5.0 (Release date: 2024-2-09)
==============
Changes: 
* Added new genre-specific scoring models for RB (mod3narr_v2, mod3exp_v2, mod3per_v2, mod3all_v2)
and Coh (mod3narr, mod3exp, mod3per, and mod3all).

writeAlizer v1.4.0 (Release date: 2024-1-30)
==============
Changes:
* Added new RB genre-specific scoring models (mod3narr, mod3exp, mod3per, mod3all).

writeAlizer v1.3.0 (Release date: 2021-11-10)
==============
Changes:
* Replaced RB/Coh model 2s with trimmed versions of model 1s with improved data pre-processing. 

writeAlizer v1.2.0 (Release date: 2020-11-11)
==============
Changes:
* Moved model objects to OSF hosting
* Added RB/Coh model 2s (trimmed version of model 1 ensembles)

writeAlizer v1.1.0 (Release date: 2020-08-03)
==============
Changes:
* Model objects now download during first predict call

writeAlizer v1.0.4 (Release date: 2020-07-29)
==============
Changes:
* Fixed ReaderBench file import

writeAlizer v1.0.3 (Release date: 2020-05-29)
==============
Changes:
* Now accepts ReaderBench output files as .csv

writeAlizer v1.0.2 (Release date: 2020-05-27)
==============
Changes:
* predict_quality() now also returns an ID variable

writeAlizer v1.0.1 (Release date: 2020-05-14)
==============
Changes:
* Added model fit objects to package and test data

writeAlizer v1.0.0 (Release date: 2020-05-01)
==============
Initial Version

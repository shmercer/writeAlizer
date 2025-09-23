## Resubmission: writeAlizer 1.6.4

This submission addresses CRAN review feedback.

- Replaced `writeAlizer:::wa_seed_example_models()` with `writeAlizer::wa_seed_example_models()` in examples and documentation, per CRAN’s guidance to avoid accessing unexported objects.

- Exported `wa_seed_example_models()` with full documentation:
  - Added `@return` (returns the created temp path invisibly).
  - Added a small, runnable example that writes only to `tempdir()` and cleans up; it also restores any prior option value.
  - Documented the temporary side effect of setting `options(writeAlizer.mock_dir = <path>)`.
  
Thank you for reviewing.

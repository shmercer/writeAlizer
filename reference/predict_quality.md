# Predict writing quality

Run the specified model(s) on preprocessed data and return predictions.
Apply scoring models to ReaderBench, Coh-Metrix, and/or GAMET files.
Holistic writing quality can be generated from ReaderBench (model =
'rb_mod3all') or Coh-Metrix files (model = 'coh_mod3all'). Also, Total
Words Written, Words Spelled Correctly, Correct Word Sequences, and
Correct Minus Incorrect Word Sequences can be generated from a GAMET
file (model = 'gamet_cws1').

## Usage

``` r
predict_quality(model, data)
```

## Arguments

- model:

  A string telling which scoring model to use. Options are: 'rb_mod1',
  'rb_mod2', 'rb_mod3narr', 'rb_mod3exp', 'rb_mod3per', or 'rb_mod3all',
  for ReaderBench files to generate holistic quality, 'coh_mod1',
  'coh_mod2', 'coh_mod3narr', 'coh_mod3exp', 'coh_mod3per', or
  'coh_mod3all' for Coh-Metrix files to generate holistic quality, and
  'gamet_cws1' to generate Total Words Written (TWW), Words Spelled
  Correctly (WSC), Correct Word Sequences (CWS) and Correct Minus
  Incorrect Word Sequences (CIWS) scores from a GAMET file.

- data:

  Data frame returned by
  [`import_gamet`](https://shmercer.github.io/writeAlizer/reference/import_gamet.md),
  [`import_coh`](https://shmercer.github.io/writeAlizer/reference/import_coh.md),
  or
  [`import_rb`](https://shmercer.github.io/writeAlizer/reference/import_rb.md).

## Value

A `data.frame` with `ID` and one column per sub-model prediction. If
multiple sub-models are used and all predictions are numeric, an
aggregate column named `pred_<model>_mean` is added (except for
"gamet_cws1").

## Details

\*\*Offline/examples:\*\* Examples use a built-in 'example' model seeded
in a temporary directory via
`writeAlizer::wa_seed_example_models("example")`, so no downloads are
attempted and checks stay fast. The temporary files created for the
example are cleaned up at the end of the `\examples{}`.

## See also

[`import_rb`](https://shmercer.github.io/writeAlizer/reference/import_rb.md),
[`import_coh`](https://shmercer.github.io/writeAlizer/reference/import_coh.md),
[`import_gamet`](https://shmercer.github.io/writeAlizer/reference/import_gamet.md)

## Examples

``` r
# Fast, offline example: seed a tiny 'example' model and predict (no downloads)
# Force offline mode for CRAN and automated checks
old_offline <- getOption("writeAlizer.offline")
options(writeAlizer.offline = TRUE)
on.exit(options(writeAlizer.offline = old_offline), add = TRUE)

coh_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
coh <- import_coh(coh_path)

mock_old <- getOption("writeAlizer.mock_dir")
ex_dir <- writeAlizer::wa_seed_example_models("example", dir = tempdir())
on.exit(options(writeAlizer.mock_dir = mock_old), add = TRUE)

out <- predict_quality("example", coh)
#> Error in .wa_load_fits_list(canonical_model): No model artifacts registered for 'example'
head(out)
#> Error: object 'out' not found

# IMPORTANT: reset mock_dir before running full demos, so real artifacts load
options(writeAlizer.mock_dir = mock_old)


# More complete demos (skipped on CRAN to keep checks fast)
# \donttest{
# If offline mode is set (e.g., by the example guard for CRAN), skip networked demos.
if (!isTRUE(getOption("writeAlizer.offline", FALSE))) {
  ### Example 1: ReaderBench output file
  file_path1 <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  rb_file <- import_rb(file_path1)
  rb_quality <- predict_quality("rb_mod3all", rb_file)
  head(rb_quality)

  ### Example 2: Coh-Metrix output file
  file_path2 <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
  coh_file <- import_coh(file_path2)
  coh_quality <- predict_quality("coh_mod3all", coh_file)
  head(coh_quality)

  ### Example 3: GAMET output file (CWS and CIWS)
  file_path3 <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
  gam_file <- import_gamet(file_path3)
  gamet_wecbm <- predict_quality("gamet_cws1", gam_file)
  head(gamet_wecbm)
} else {
  # Skipped because writeAlizer.offline = TRUE (e.g., on CRAN)
}
#> ℹ Downloaded model artifact:
#> * File: rb_exp_vars_v2.rds
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: rb_narr_vars_v2.rds
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: rb_per_vars_v2.rds
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> Warning: These variables have zero variances: RB.SenAllit, RB.AvgDepsBl_csubj, RB.AvgDepsBl_iobj
#> Warning: These variables have zero variances: RB.AvgConnBl_complex_subordinators, RB.SenAllit, RB.AvgDepsBl_csubj, RB.AvgDepsBl_iobj, RB.AvgDepsSen_iobj
#> Warning: These variables have zero variances: RB.AvgConnBl_complex_subordinators, RB.SenAllit, RB.AvgDepsBl_csubj, RB.AvgDepsSen_csubj, RB.AvgDepsBl_iobj
#> ℹ Downloaded model artifact:
#> * File: rb_mod3exp_v2.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: rb_mod3narr_v2.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: rb_mod3per_v2.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: coh_exp_vars.rds
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: coh_narr_vars.rds
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: coh_per_vars.rds
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> Warning: These variables have zero variances: WRDPRP1p
#> Warning: These variables have zero variances: WRDPRP1p
#> Warning: These variables have zero variances: WRDPRP1p
#> ℹ Downloaded model artifact:
#> * File: coh_mod3exp.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: coh_mod3narr.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: coh_mod3per.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: CWS_mod1a.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#> ℹ Downloaded model artifact:
#> * File: CIWS_mod1a.rda
#> * Cache: /home/runner/.cache/R/writeAlizer
#>   (Artifacts are downloaded only the first time you use a model.)
#>   Tip: clear the cache with wa_cache_clear() if needed.
#>   ID pred_TWW_gamet pred_WSC_gamet pred_CWS_mod1a pred_CIWS_mod1a
#> 1  7            135            129       125.8289        107.8254
#> 2  8            171            168       157.8647        141.8125
#> 3  9            191            189       168.2293        149.5521
# }
```

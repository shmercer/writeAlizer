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
# Offline, CRAN-safe example using a tiny seeded model
if (requireNamespace("withr", quietly = TRUE)) {
  withr::local_options(writeAlizer.offline = TRUE)
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  # Seed the example artifacts into the temp dir and point the loader there
  writeAlizer::wa_seed_example_models("example", dir = tmp)

  coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
  out <- predict_quality("example", coh)
  head(out)
} else {
  # Fallback without 'withr' (still CRAN-safe)
  old <- options(writeAlizer.offline = TRUE)
  on.exit(options(old), add = TRUE)
  ex_dir <- writeAlizer::wa_seed_example_models("example", dir = tempdir())
  old2 <- options(writeAlizer.mock_dir = ex_dir)
  on.exit(options(old2), add = TRUE)

  coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
  out <- predict_quality("example", coh)
  head(out)
}
#>   ID pred_example
#> 1  7          1.5
#> 2  8          1.5
#> 3  9          1.5

# Longer, networked demos (skipped on CRAN)
# \donttest{
if (!isTRUE(getOption("writeAlizer.offline", FALSE))) {
  rb <- import_rb(system.file("extdata", "sample_rb.csv", package = "writeAlizer"))
  print(head(predict_quality("rb_mod3all", rb)))

  coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
  print(head(predict_quality("coh_mod3all", coh)))

  gam <- import_gamet(system.file("extdata", "sample_gamet.csv", package = "writeAlizer"))
  print(head(predict_quality("gamet_cws1", gam)))
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
#>   ID pred_rb_mod3exp pred_rb_mod3narr pred_rb_mod3per pred_rb_mod3all_mean
#> 1  7      -1.4434553      -1.69349246      -0.9573641          -1.36477065
#> 2  8       0.0796015      -0.03163491      -0.2293523          -0.06046189
#> 3  9       1.4068790       1.80142661       1.3995404           1.53594866
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
#>   ID pred_coh_mod3exp pred_coh_mod3narr pred_coh_mod3per pred_coh_mod3all_mean
#> 1  7       -2.5052929         -3.496357       -2.9239837            -2.9752112
#> 2  8        0.5765171          1.436159        0.6002912             0.8709891
#> 3  9        1.6753017          2.488845        3.3214388             2.4951951
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

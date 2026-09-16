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

  A string telling which scoring model to use. ReaderBench Model 3 keys
  also accept a '\_v2' suffix. The 'example' key is an offline
  demonstration. Options are: 'rb_mod1', 'rb_mod2', 'rb_mod3narr',
  'rb_mod3exp', 'rb_mod3per', or 'rb_mod3all', for ReaderBench files to
  generate holistic quality, 'coh_mod1', 'coh_mod2', 'coh_mod3narr',
  'coh_mod3exp', 'coh_mod3per', or 'coh_mod3all' for Coh-Metrix files to
  generate holistic quality, and 'gamet_cws1' to generate Total Words
  Written (TWW), Words Spelled Correctly (WSC), Correct Word Sequences
  (CWS) and Correct Minus Incorrect Word Sequences (CIWS) scores from a
  GAMET file.

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
"gamet_cws1"). Missing component scores are omitted from the mean; an
entirely missing row yields NaN. GAMET returns `pred_TWW_gamet`,
`pred_WSC_gamet`, `pred_CWS_mod1a`, and `pred_CIWS_mod1a`. Predictions
are not rounded or clipped.

## Details

Models 2 and 3 center and scale features using the data supplied in this
call. Changing the scoring group can change a text's score. A single row
or features with no variation can produce missing values. Model 1 and
GAMET pass the input through to their saved models without this
additional scaling.

The 'example' model is for demonstrating the workflow only. Its
preprocessing needs no downloads; prediction requires
[`wa_seed_example_models()`](https://shmercer.github.io/writeAlizer/reference/wa_seed_example_models.md)
first. The temporary files created for the example are cleaned up at the
end of the `\examples{}`.

## See also

[`import_rb`](https://shmercer.github.io/writeAlizer/reference/import_rb.md),
[`import_coh`](https://shmercer.github.io/writeAlizer/reference/import_coh.md),
[`import_gamet`](https://shmercer.github.io/writeAlizer/reference/import_gamet.md)

## Examples

``` r
local({
  old <- options(writeAlizer.mock_dir = NULL, writeAlizer.offline = TRUE)
  on.exit(options(old))
  parent <- tempfile("wa-example-")
  wa_seed_example_models(dir = parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
  head(predict_quality("example", coh))
})
#>   ID pred_example
#> 1  7          1.5
#> 2  8          1.5
#> 3  9          1.5

# Longer, networked demos
if (FALSE) { # \dontrun{
if (!isTRUE(getOption("writeAlizer.offline", FALSE))) {
  rb <- import_rb(system.file("extdata", "sample_rb.csv", package = "writeAlizer"))
  print(head(predict_quality("rb_mod3all", rb)))

  coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
  print(head(predict_quality("coh_mod3all", coh)))

  gam <- import_gamet(system.file("extdata", "sample_gamet.csv", package = "writeAlizer"))
  print(head(predict_quality("gamet_cws1", gam)))
}
} # }
```

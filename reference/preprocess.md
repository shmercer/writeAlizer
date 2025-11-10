# Pre-process data

Pre-process Coh-Metrix and ReaderBench data files before applying
predictive models. Uses the artifact registry to load the correct
variable lists and applies centering and scaling per sub-model,
preserving the original behavior by model key.

## Usage

``` r
preprocess(model, data)
```

## Arguments

- model:

  Character scalar. Which scoring model to use. Supported values
  include: ReaderBench:
  'rb_mod1','rb_mod2','rb_mod3narr','rb_mod3exp','rb_mod3per','rb_mod3all',
  'rb_mod3narr_v2','rb_mod3exp_v2','rb_mod3per_v2','rb_mod3all_v2';
  Coh-Metrix:
  'coh_mod1','coh_mod2','coh_mod3narr','coh_mod3exp','coh_mod3per','coh_mod3all';
  GAMET: 'gamet_cws1'. Legacy keys for RB mod3 (non-v2) are mapped to
  their v2 equivalents internally.

- data:

  A data.frame produced by
  [`import_rb`](https://shmercer.github.io/writeAlizer/reference/import_rb.md),
  [`import_coh`](https://shmercer.github.io/writeAlizer/reference/import_coh.md),
  or
  [`import_gamet`](https://shmercer.github.io/writeAlizer/reference/import_gamet.md),
  with an `ID` column and the expected feature columns.

## Value

A list of pre-processed data frames, one per sub-model. For models with
no varlists (e.g., 'rb_mod1','coh_mod1'), returns six copies of the
input data. For 'gamet_cws1', returns two copies (CWS/CIWS). For
1-part/3-part models, returns a list of length 1/3 with centered &
scaled features plus the `ID` column.

## Details

\*\*Offline/examples:\*\* Examples use a built-in 'example' model seeded
in a temporary directory via
`writeAlizer::wa_seed_example_models("example")`, so no downloads are
attempted and checks stay fast.

## Examples

``` r
# Minimal, offline example using the built-in 'example' model (no downloads)
rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
rb <- import_rb(rb_path)

pp <- preprocess("example", rb)
length(pp); lapply(pp, nrow)
#> [1] 1
#> [[1]]
#> [1] 3
#> 
```

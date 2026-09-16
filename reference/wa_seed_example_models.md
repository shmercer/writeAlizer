# Seed example model files in a temporary directory

This helper writes a minimal model file to a subdirectory of \`dir\`
(default: \`tempdir()\`), and sets the option \`writeAlizer.mock_dir\`
to that location so examples can run without downloads or network
access.

## Usage

``` r
wa_seed_example_models(model = c("example"), dir = tempdir())
```

## Arguments

- model:

  Character scalar. Only \`"example"\` is currently supported.

- dir:

  Directory in which to create the example model (default:
  \`tempdir()\`).

## Value

(Invisibly) the path to the created example model directory.

## Details

Creates an ultra-tiny model artifact used in examples and points the
package loader to it via a temporary option.

\- Writes under the supplied \`dir\` (by default \`tempdir()\`) and
returns the path. - The example predicts a constant 1.5; it is not a
writing assessment. - Sets \`options(writeAlizer.mock_dir = \<path\>)\`;
callers should restore prior options when appropriate (see Examples).

## Examples

``` r
local({
  old <- options(writeAlizer.mock_dir = NULL)
  on.exit(options(old))
  parent <- tempfile("wa-example-")
  ex <- wa_seed_example_models(dir = parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  predict_quality("example", data.frame(ID = c("text1", "text2")))
})
#>      ID pred_example
#> 1 text1          1.5
#> 2 text2          1.5
```

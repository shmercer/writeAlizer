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

\- Writes only under \`tempdir()\` and returns the created path. - Sets
\`options(writeAlizer.mock_dir = \<path\>)\`; callers should restore
prior options when appropriate (see Examples).

## Examples

``` r
old <- getOption("writeAlizer.mock_dir")
on.exit(options(writeAlizer.mock_dir = old), add = TRUE)

ex <- wa_seed_example_models(dir = tempdir())
# Use the package normally here; the loader will find `ex`
# ...
unlink(ex, recursive = TRUE, force = TRUE)
```

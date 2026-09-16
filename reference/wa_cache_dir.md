# Path to writeAlizer's user cache

Returns the directory used to store cached model artifacts. By default
this is a platform-appropriate user cache path from
`tools::R_user_dir("writeAlizer","cache")`. If the option
`writeAlizer.cache_dir` is set to a non-empty string, that location is
used instead. This makes it easy to redirect the cache during tests or
examples (e.g., to [`tempdir()`](https://rdrr.io/r/base/tempfile.html)).

## Usage

``` r
wa_cache_dir()
```

## Value

Character scalar path.

## Details

The override must be one non-missing character string. An empty string
uses the default. Choose a dedicated directory: clearing the cache
deletes all of its contents.

## See also

[`wa_cache_clear`](https://shmercer.github.io/writeAlizer/reference/wa_cache_clear.md)

## Examples

``` r
wa_cache_dir()  # Inspect the path without creating or deleting files
#> [1] "/home/runner/.cache/R/writeAlizer"
```

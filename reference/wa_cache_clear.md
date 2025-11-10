# Clear writeAlizer's user cache

Deletes all files under
[`wa_cache_dir()`](https://shmercer.github.io/writeAlizer/reference/wa_cache_dir.md).
If `ask = TRUE` *and* in an interactive session, a short preview (item
count, total size, and up to 10 sample paths) is printed before asking
for confirmation.

## Usage

``` r
wa_cache_clear(ask = interactive(), preview = TRUE)
```

## Arguments

- ask:

  Logical; if `TRUE` and interactive, ask for confirmation.

- preview:

  Logical; if `TRUE` and `ask` is `TRUE`, show a brief listing/size
  summary before asking.

## Value

Invisibly returns `TRUE` if the cache was cleared (or already absent),
`FALSE` if the user declined or deletion failed.

## See also

[`wa_cache_dir`](https://shmercer.github.io/writeAlizer/reference/wa_cache_dir.md)

## Examples

``` r
# Safe demo: redirect cache to tempdir(), create a file, then clear it
#> Cleared cache: /tmp/RtmpZ2Y5Hh/wa_cache_demo2
```

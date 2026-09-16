# Clear writeAlizer's user cache

Deletes all files under
[`wa_cache_dir()`](https://shmercer.github.io/writeAlizer/reference/wa_cache_dir.md).
If `ask = TRUE` *and* in an interactive session, a short preview (item
count, total size, and up to 10 sample paths) is printed before asking
for confirmation. In a non-interactive script, no prompt is shown, even
if `ask = TRUE`. Use this only on a dedicated cache folder; all its
contents are removed.

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
local({
  old <- options(writeAlizer.cache_dir = tempfile("wa-cache-"))
  on.exit(options(old))
  dir.create(wa_cache_dir())
  on.exit(unlink(wa_cache_dir(), recursive = TRUE), add = TRUE, after = FALSE)
  writeLines("demo", file.path(wa_cache_dir(), "demo.txt"))
  wa_cache_clear(ask = FALSE)
})
#> Cleared cache: /tmp/RtmpZPeAkE/wa-cache-192350a31099
```

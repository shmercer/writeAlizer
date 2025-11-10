# Download and cache an artifact (graceful offline behavior)

Public helper to fetch an artifact into the user cache. This function
delegates to the internal downloader used by the package at runtime, so
it benefits from the same behavior:

## Usage

``` r
wa_download(file, url, sha256 = NULL, quiet = TRUE)

download(file, url) # deprecated
```

## Arguments

- file:

  Character scalar; filename to use in the cache (e.g.,
  \`"rb_mod1a.rda"\`).

- url:

  Character scalar; source URL. May be a \`file://\` URL for local
  testing.

- sha256:

  Optional 64-hex SHA-256 checksum for verification. If provided, the
  cached file must match it (or a re-download is attempted).

- quiet:

  Logical; if \`TRUE\`, suppresses download progress messages.

## Value

A character scalar: the absolute path to the cached file.

## Details

\- Respects `options(writeAlizer.mock_dir)` to load local mock copies
(useful for tests/examples and offline runs). - Fails *gracefully* with
a clear, informative message when Internet resources are unavailable or
have changed (per CRAN policy). - Verifies an optional SHA-256 checksum
and re-downloads or errors if it does not match.

## Examples

``` r
# Offline-friendly example using a local source (no network):
src <- tempfile(fileext = ".bin")
writeBin(as.raw(1:10), src)
dest <- wa_download(
  "example.bin",
  url = paste0("file:///", normalizePath(src, winslash = "/"))
)
file.exists(dest)
#> [1] TRUE

# Using a mock directory to avoid network access:
# options(writeAlizer.mock_dir = "/path/to/local/artifacts")
# dest <- wa_download("rb_mod1a.rda", url = "https://example.com/rb_mod1a.rda")
```

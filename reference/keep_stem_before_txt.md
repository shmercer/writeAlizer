# Extract the filename stem before ".txt"

Removes any directory path and optional \`.txt\` extension from
filenames or file paths. This function standardizes text identifiers
across Coh-Metrix, GAMET, and other text analysis outputs that may
include full paths or extensions in their ID fields.

## Usage

``` r
keep_stem_before_txt(x)
```

## Arguments

- x:

  A character vector (or coercible) containing file paths or filenames.
  Elements may or may not include a \`.txt\` suffix or any directory
  path.

## Value

A character vector where each element is reduced to the final path
component, with any trailing \`.txt\` (case-insensitive) removed. \`NA\`
values are preserved as \`NA_character\_\`.

## Details

The function handles both forward (\`/\`) and backward (\`\\) slashes in
file paths. If a value has no path and/or no \`.txt\` suffix, it is
returned unchanged (aside from coercion to character).

## Examples

``` r
keep_stem_before_txt(c(
  "C:/data/3401.txt",
  "E:\\\\samples\\\\1002.TXT",
  "plain_id",
  NA
))
#> [1] "3401"     "1002"     "plain_id" NA        
#> [1] "3401" "1002" "plain_id" NA
```

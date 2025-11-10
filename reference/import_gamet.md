# Import a GAMET output file into R.

Import a GAMET output file into R.

## Usage

``` r
import_gamet(path)
```

## Arguments

- path:

  A string giving the path and filename to import.

## Value

A base `data.frame` with one row per record and the following columns:

- `ID` (`character`): unique identifier of the text/essay.

- One column per retained GAMET error/category variable (`numeric`;
  typically counts or rates). Column names follow the GAMET output
  variable names.

The object has class `data.frame` (or `tibble` if converted by the
user).

## See also

[`predict_quality`](https://shmercer.github.io/writeAlizer/reference/predict_quality.md)

## Examples

``` r
# Example with package sample data
file_path   <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
gamet_file  <- import_gamet(file_path)
head(gamet_file)
#>   ID error_count word_count grammar misspelling duplication typographical
#> 1  7           6        135       0           6           0             0
#> 2  8           4        171       0           3           0             1
#> 3  9           3        191       0           2           0             1
#>   whitespace per_gram  per_spell
#> 1          0        0 0.04444444
#> 2          0        0 0.01754386
#> 3          0        0 0.01047120
```

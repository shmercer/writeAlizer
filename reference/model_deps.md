# Report optional model dependencies (no installation performed)

Discovers package dependencies for model fitting from the package
\`Suggests\` field. This function \*\*never installs\*\* packages. It
reports which packages are required and which are currently missing, and
prints a ready-to-copy command you can run to install the missing ones
manually.

## Usage

``` r
model_deps()
```

## Value

A named list:

- required:

  Character vector of discovered package tokens (may include version
  qualifiers), e.g. `c("glmnet (>= 4.1)", "ranger")`. This is the union
  of the package *Suggests* field and the optional
  `writeAlizer.required_pkgs` override.

- missing:

  Character vector of base package names that are not installed, e.g.
  `c("glmnet", "ranger")`.

The function also emits a message. If nothing is missing, it reports
that all required packages are installed. Otherwise, it lists the
missing packages and prints a copy-paste
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
command.

## Details

You can add or override discovered packages for testing or CI with
\`options(writeAlizer.required_pkgs = c("pkgA", "pkgB (\>= 1.2.3)"))\`.
Any version qualifiers you include are preserved in the \`required\`
output, but stripped for the availability check in \`missing\`.

## Examples

``` r
md <- model_deps()
#> ✔ All required packages are installed: caretEnsemble, Cubist, curl, earth, gbm, glmnet, kernlab, knitr, pls, randomForest, rmarkdown, testthat, withr
md$missing
#> character(0)

#> ✖ Missing required packages:
#> • thispkgdoesnotexist123
#> • another.fake
#> install.packages(c("thispkgdoesnotexist123", "another.fake"))
```

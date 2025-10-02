# tests/testthat/helper-sample-paths.R
# Resolve paths to sample files whether running against an installed build
# or the source tree (devtools::test()).
wa_sample_path <- function(fname) {
  stopifnot(is.character(fname), length(fname) == 1L, nzchar(fname))

  candidates <- character(0)

  # 1) Installed package (preferred)
  p1 <- system.file("extdata", fname, package = "writeAlizer")
  if (nzchar(p1)) candidates <- c(candidates, p1)

  # 2a) Source tree (pkg root is two levels above tests/testthat/)
  #     tests/testthat/../../inst/extdata/<fname>
  candidates <- c(candidates, testthat::test_path("..", "..", "inst", "extdata", fname))

  # 2b) Some runners structure differently; also try one-level up
  #     tests/../inst/extdata/<fname>
  candidates <- c(candidates, testthat::test_path("..", "inst", "extdata", fname))

  # 3) Direct relative fallback (when running from pkg root)
  candidates <- c(candidates, file.path("inst", "extdata", fname))

  # Normalize and return first that exists
  for (p in unique(candidates)) {
    if (nzchar(p) && file.exists(p)) {
      return(normalizePath(p, mustWork = FALSE))
    }
  }
  ""  # signal "not found"
}

# Optional convenience wrappers
wa_sample_rb   <- function() wa_sample_path("sample_rb.csv")
wa_sample_coh  <- function() wa_sample_path("sample_coh.csv")
wa_sample_gam  <- function() wa_sample_path("sample_gamet.csv")

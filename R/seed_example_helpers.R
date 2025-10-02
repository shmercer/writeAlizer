#' Seed example model files in a temporary directory
#'
#' Creates an ultra-tiny model artifact used in examples and points the
#' package loader to it via a temporary option.
#'
#' @description
#' This helper writes a minimal model file to a subdirectory of `dir`
#' (default: `tempdir()`), and sets the option
#' `writeAlizer.mock_dir` to that location so examples can run
#' without downloads or network access.
#'
#' @details
#' - Writes only under `tempdir()` and returns the created path.
#' - Sets `options(writeAlizer.mock_dir = <path>)`; callers should
#'   restore prior options when appropriate (see Examples).
#'
#' @param model Character scalar. Only `"example"` is currently supported.
#' @param dir Directory in which to create the example model (default: `tempdir()`).
#'
#' @return
#' (Invisibly) the path to the created example model directory.
#'
#' @examples
#' old <- getOption("writeAlizer.mock_dir")
#' on.exit(options(writeAlizer.mock_dir = old), add = TRUE)
#'
#' ex <- wa_seed_example_models(dir = tempdir())
#' # Use the package normally here; the loader will find `ex`
#' # ...
#' unlink(ex, recursive = TRUE, force = TRUE)
#'
#' @export
wa_seed_example_models <- function(model = c("example"), dir = tempdir()) {
  model <- match.arg(model)

  exdir <- file.path(dir, "writeAlizer_example")
  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE, showWarnings = FALSE)

  # ultra-tiny intercept-only model; no external datasets
  df  <- data.frame(y = c(1, 2))
  fit <- stats::lm(y ~ 1, data = df)
  save(fit, file = file.path(exdir, "example.rda"))

  # tell the loader to look here first
  options(writeAlizer.mock_dir = exdir)

  invisible(exdir)
}

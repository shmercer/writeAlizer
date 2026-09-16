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
#' - Writes under the supplied `dir` (by default `tempdir()`) and returns the path.
#' - The example predicts a constant 1.5; it is not a writing assessment.
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
#' local({
#'   old <- options(writeAlizer.mock_dir = NULL)
#'   on.exit(options(old))
#'   parent <- tempfile("wa-example-")
#'   ex <- wa_seed_example_models(dir = parent)
#'   on.exit(unlink(parent, recursive = TRUE), add = TRUE)
#'   predict_quality("example", data.frame(ID = c("text1", "text2")))
#' })
#'
#' @export
wa_seed_example_models <- function(model = c("example"), dir = tempdir()) {
  model <- match.arg(model)
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    rlang::abort("`dir` must be a non-empty directory path.", .subclass = "writeAlizer_input_error")
  }

  exdir <- file.path(dir, "writeAlizer_example")
  if (!dir.exists(exdir) && !dir.create(exdir, recursive = TRUE, showWarnings = FALSE)) {
    rlang::abort(paste0("Cannot create example directory: ", exdir), .subclass = "writeAlizer_input_error")
  }

  # ultra-tiny intercept-only model; no external datasets
  df  <- data.frame(y = c(1, 2))
  fit <- stats::lm(y ~ 1, data = df)
  save(fit, file = file.path(exdir, "example.rda"))

  # tell the loader to look here first
  options(writeAlizer.mock_dir = exdir)

  invisible(exdir)
}

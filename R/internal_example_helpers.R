#' Seed tiny mock model files for examples and point the loader to them.
#' Internal-only; used in examples.
#' @keywords internal
#' @noRd
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

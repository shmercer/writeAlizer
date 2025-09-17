#' Seed tiny mock model files for examples and point the loader to them.
#' Internal-only; used in examples.
#' @keywords internal
#' @noRd
wa_seed_example_models <- function(model = c("example"), dir = tempdir()) {
  model <- match.arg(model)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # tiny intercept-only model; no external datasets needed
  df  <- data.frame(y = c(1, 2))
  fit <- stats::lm(y ~ 1, data = df)
  save(fit, file = file.path(dir, "example.rda"))

  options(writeAlizer.mock_dir = dir)
  invisible(dir)
}

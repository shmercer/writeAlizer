# tests/testthat/test-artifact-registry-ensure-file.R

# Helper: run an expression while capturing ALL console output (stdout + messages),
# and return the expression's value.
.with_quiet_console <- function(expr) {
  tmp_out <- tempfile()
  tmp_msg <- tempfile()
  on.exit(unlink(c(tmp_out, tmp_msg), recursive = FALSE, force = TRUE), add = TRUE)

  con_out <- file(tmp_out, open = "wt")
  con_msg <- file(tmp_msg, open = "wt")
  sink(con_out, type = "output")
  sink(con_msg, type = "message")

  # Ensure we always restore sinks, even if the code errors
  on.exit({
    # Drain message sinks
    repeat {
      ok <- try({ sink(type = "message"); FALSE }, silent = TRUE)
      if (isFALSE(ok)) break
    }
    # Drain output sinks
    repeat {
      ok <- try({ sink(type = "output"); FALSE }, silent = TRUE)
      if (isFALSE(ok)) break
    }
    try(close(con_msg), silent = TRUE)
    try(close(con_out), silent = TRUE)
  }, add = TRUE)

  force(expr)
}

testthat::test_that(".wa_ensure_file downloads via file:// URL and caches result", {
  ensure_file   <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path   <- getFromNamespace(".wa_cached_path", "writeAlizer")

  # Prefer a quieter download backend if available
  withr::local_options(download.file.method = "libcurl")

  # Source artifact: tiny, valid .rda saved to a temp file
  tmp_src <- withr::local_tempfile(fileext = ".rda")
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  save(fit, file = tmp_src)
  url <- paste0("file:///", normalizePath(tmp_src, winslash = "/"))

  # Redirect cache to a temp dir for this test (cross-platform)
  tmp_cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = tmp_cache)

  # Use a nested relative path to exercise dir creation
  rel_name <- file.path("models", "coh", "tiny_fit.rda")

  # Ensure the destination parent directory exists (avoid destfile errors)
  dest <- cached_path(rel_name)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  # 1) First call: downloads (copies) to cache, but do it quietly
  p1 <- .with_quiet_console(ensure_file(rel_name, url = url))
  testthat::expect_true(file.exists(p1))
  testthat::expect_identical(normalizePath(p1), normalizePath(dest))
  testthat::expect_identical(basename(p1), basename(rel_name))

  # 2) Second call: should reuse cached copy (same path); also quiet
  p2 <- .with_quiet_console(ensure_file(rel_name, url = url))
  testthat::expect_identical(normalizePath(p1), normalizePath(p2))
})

testthat::test_that(".wa_ensure_file errors when neither mock_dir nor source exist", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")

  # Prefer a quieter download backend if available
  withr::local_options(download.file.method = "libcurl")

  # Redirect cache to isolated temp dir
  tmp_cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = tmp_cache)

  rel_name <- file.path("missing_dir", "missing_file.rda")
  dest <- cached_path(rel_name)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  bad_url <- "file:///this/definitely/does/not/exist.rda"

  # Silence console output while we assert the error
  testthat::expect_error(
    .with_quiet_console(suppressWarnings(ensure_file(rel_name, url = bad_url))),
    regexp = "cannot open URL|nonzero exit status|download|ensure",
    ignore.case = TRUE
  )
})

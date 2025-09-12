# tests/testthat/test-artifact-registry-ensure-file.R

testthat::test_that(".wa_ensure_file downloads via file:// URL and caches result", {
  ensure_file   <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path   <- getFromNamespace(".wa_cached_path", "writeAlizer")

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

  # Ensure the destination parent directory exists (avoid download.file destfile error)
  dest <- cached_path(rel_name)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  # 1) First call: downloads (copies) to cache
  p1 <- ensure_file(rel_name, url = url)
  testthat::expect_true(file.exists(p1))
  testthat::expect_identical(normalizePath(p1), normalizePath(dest))
  testthat::expect_identical(basename(p1), basename(rel_name))

  # 2) Second call: should reuse cached copy (same path)
  p2 <- ensure_file(rel_name, url = url)
  testthat::expect_identical(normalizePath(p1), normalizePath(p2))
})

testthat::test_that(".wa_ensure_file errors when neither mock_dir nor source exist", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")

  # Redirect cache to isolated temp dir
  tmp_cache <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = tmp_cache)

  rel_name <- file.path("missing_dir", "missing_file.rda")
  dest <- cached_path(rel_name)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  bad_url <- "file:///this/definitely/does/not/exist.rda"

  # Suppress the download.file warning; we assert the resulting error
  testthat::expect_error(
    suppressWarnings(ensure_file(rel_name, url = bad_url)),
    regexp = "cannot open URL|nonzero exit status|download|ensure",
    ignore.case = TRUE
  )
})

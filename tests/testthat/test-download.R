withr::local_options(writeAlizer.offline = FALSE, writeAlizer.mock_dir = NULL)

testthat::test_that("wa_download() succeeds (file://) and reuses cache", {
  tmp <- withr::local_tempdir()
  src <- file.path(tmp, "tiny.bin")
  writeBin(as.raw(1:10), src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  sha <- digest::digest(src, algo = "sha256", file = TRUE)

  # 1) first download
  p1 <- writeAlizer::wa_download("tiny.bin", url, sha256 = sha, quiet = TRUE)
  testthat::expect_true(file.exists(p1))

  # 2) second download should hit cache (same path)
  p2 <- writeAlizer::wa_download("tiny.bin", url, sha256 = sha, quiet = TRUE)
  testthat::expect_identical(normalizePath(p1), normalizePath(p2))
})

testthat::test_that("wa_download() enforces checksum mismatches", {
  tmp <- withr::local_tempdir()
  src <- file.path(tmp, "tiny2.bin")
  writeBin(as.raw(1:5), src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  bad_sha <- paste(rep("0", 64), collapse = "")
  # wa_download() warns about mismatch and then errors; suppress the expected
  # warning here so the test summary stays clean (no WARN line).
  suppressWarnings(
    testthat::expect_error(
      writeAlizer::wa_download("tiny2.bin", url, sha256 = bad_sha, quiet = TRUE),
      "Checksum|SHA", ignore.case = TRUE
    )
  )
})

testthat::test_that("deprecated download() wrapper still works (no warning noise)", {
  tmp <- withr::local_tempdir()
  src <- file.path(tmp, "legacy.bin")
  writeBin(as.raw(1:5), src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  p <- suppressWarnings(writeAlizer::download("legacy.bin", url))
  testthat::expect_true(file.exists(p))
})

testthat::test_that(".wa_ensure_file() uses mock_dir and copies deterministically", {
  testthat::skip_on_cran()

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)

  withr::local_options(writeAlizer.mock_dir = withr::local_tempdir())
  mock_dir <- getOption("writeAlizer.mock_dir")

  # Create a nested mock artifact that mimics registry layout
  rel <- file.path("models", "example", "artifact.bin")
  src <- file.path(mock_dir, rel)
  dir.create(dirname(src), recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(1:10), src) # deterministic content

  # Call should copy from mock_dir and ignore URL entirely when present
  dest <- writeAlizer:::.wa_ensure_file(file = rel, url = "file://ignored")
  testthat::expect_true(file.exists(dest))

  # Validate content copied
  testthat::expect_identical(digest::digest(dest, algo = "sha256", file = TRUE),
                             digest::digest(src,  algo = "sha256", file = TRUE))
})

testthat::test_that(".wa_ensure_file() respects offline mode", {
  testthat::skip_on_cran()

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)

  withr::local_options(writeAlizer.offline = TRUE,
                       writeAlizer.mock_dir = NULL)

  # No cache and offline => typed error (message may vary, match stable fragment)
  rel <- "does_not_exist.bin"
  testthat::expect_error(
    writeAlizer:::.wa_ensure_file(file = rel, url = "file:///definitely/missing.bin"),
    class  = "writeAlizer_download_missing",
    regexp = "Missing file for URL"
  )
})

testthat::test_that(".wa_ensure_file() errors when file:// source missing", {
  testthat::skip_on_cran()

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)

  withr::local_options(writeAlizer.offline = FALSE,
                       writeAlizer.mock_dir = NULL)

  rel <- "really_missing.bin"
  testthat::expect_error(
    writeAlizer:::.wa_ensure_file(file = rel, url = "file:///nope/really_missing.bin", max_retries = 0),
    class  = "writeAlizer_download_missing",
    regexp = "Missing file for URL 'file:///nope/really_missing.bin'"
  )
})



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

test_that(".wa_ensure_file() uses mock_dir and copies deterministically", {
  skip_on_cran()

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
  dest <- .wa_ensure_file(file = rel, url = "file://ignored")
  expect_true(file.exists(dest))

  # Validate content copied
  expect_identical(digest::digest(dest, algo = "sha256", file = TRUE),
                   digest::digest(src,  algo = "sha256", file = TRUE))
})

test_that(".wa_ensure_file() respects offline mode", {
  skip_on_cran()

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)

  withr::local_options(writeAlizer.offline = TRUE,
                       writeAlizer.mock_dir = NULL)

  # No cache and offline => typed error
  rel <- "models/x/does_not_exist.bin"
  expect_error(
    .wa_ensure_file(file = rel, url = "file:///definitely/missing.bin"),
    class = "writeAlizer_offline_error"
  )
})

test_that(".wa_ensure_file() errors when file:// source missing", {
  skip_on_cran()

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)

  withr::local_options(writeAlizer.offline = FALSE,
                       writeAlizer.mock_dir = NULL)

  rel <- "models/example/missing.bin"
  # Source does not exist
  expect_error(
    .wa_ensure_file(file = rel, url = "file:///nope/really_missing.bin", max_retries = 0),
    class = "writeAlizer_artifact_missing"
  )
})

test_that(".wa_ensure_file() re-downloads on checksum mismatch; fails when still bad", {
  skip_on_cran()

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)

  withr::local_options(writeAlizer.offline = FALSE,
                       writeAlizer.mock_dir = NULL)

  # Prepare a source artifact with known SHA
  src_dir  <- withr::local_tempdir()
  src_path <- file.path(src_dir, "good.bin")
  writeBin(as.raw(rep(42L, 32)), src_path)
  good_sha <- digest::digest(src_path, algo = "sha256", file = TRUE)

  rel <- "models/example/good.bin"

  # First: create a WRONG cached file to trigger "cached checksum mismatch"
  cached <- file.path(cache_dir, rel)
  dir.create(dirname(cached), recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(rep(1L, 8)), cached) # wrong contents

  # Now: call ensure with an intentionally WRONG sha to force post-download mismatch
  bad_sha <- paste(rev(strsplit(good_sha, "")[[1]]), collapse = "")

  # Suppress the expected warning ("Checksum mismatch for cached ... Re-downloading.")
  suppressWarnings(
    expect_error(
      .wa_ensure_file(
        file = rel,
        url  <- to_file_url(src_path),
        sha256 = bad_sha,
        max_retries = 0
      ),
      class = "writeAlizer_checksum_mismatch"
    )
  )

  # Calling with the correct sha should succeed and leave correct bits on disk
  out <- .wa_ensure_file(
    file = rel,
    url  <- to_file_url(src_path),
    sha256 = good_sha,
    max_retries = 0
  )
  expect_true(file.exists(out))
  expect_identical(digest::digest(out, algo = "sha256", file = TRUE), good_sha)
})

test_that(".wa_ensure_file() input validation and return type", {
  skip_on_cran()
  expect_error(.wa_ensure_file(file = "", url = "file://x"), class = "writeAlizer_input_error")
  expect_error(.wa_ensure_file(file = "x", url = ""), class = "writeAlizer_input_error")

  # Happy path via file:// URL
  src_dir <- withr::local_tempdir()
  src_path <- file.path(src_dir, "a.bin")
  writeBin(as.raw(1:3), src_path)
  rel <- "models/a/a.bin"
  out <- .wa_ensure_file(rel, url <- to_file_url(src_path))
  expect_true(is.character(out) && length(out) == 1L && file.exists(out))
})


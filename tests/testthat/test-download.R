# tests/testthat/test-download.R

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

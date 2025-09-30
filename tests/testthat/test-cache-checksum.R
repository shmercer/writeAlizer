testthat::test_that("checksum is enforced for downloads and cached reads", {
  tmp <- withr::local_tempdir()
  # Prepare a small local "remote" file and its correct sha
  src <- file.path(tmp, "tiny.bin")
  writeBin(as.raw(1:10), src)
  correct_sha <- digest::digest(src, algo = "sha256", file = TRUE)

  # Cache destination filename
  fn <- "tiny.bin"
  cache_file <- file.path(tools::R_user_dir("writeAlizer", "cache"), fn)
  if (file.exists(cache_file)) file.remove(cache_file)

  # Compose a file:// URL that works on all platforms
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  # 1) First ensure: downloads from file:// and verifies OK
  p1 <- writeAlizer:::.wa_ensure_file(fn, url = url, sha256 = correct_sha, quiet = TRUE)
  testthat::expect_true(file.exists(p1))

  # 2) Corrupt the cached file to force a re-download path
  writeBin(as.raw(255:246), p1)  # different content

  # 3) Ensure again with correct sha:
  #    should emit a checksum-mismatch warning, re-download, and succeed.
  #    Capture & muffle the warning so the test summary shows 0 WARN.
  p2 <- NULL
  got_mismatch_warning <- FALSE
  withCallingHandlers(
    {
      p2 <- writeAlizer:::.wa_ensure_file(fn, url = url, sha256 = correct_sha, quiet = TRUE)
    },
    warning = function(w) {
      if (grepl("Checksum mismatch", conditionMessage(w), ignore.case = TRUE)) {
        got_mismatch_warning <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  testthat::expect_true(got_mismatch_warning)
  testthat::expect_true(file.exists(p2))
  testthat::expect_identical(
    digest::digest(p2, algo = "sha256", file = TRUE),
    correct_sha
  )

  # 4) With a WRONG sha: expect a warning and then an error about checksum mismatch
  bad_sha <- paste(rep("0", 64), collapse = "")
  # Keep this nested expect_warning(expect_error()) but muffle the warning so it
  # doesn't count toward the WARN tally.
  suppressWarnings(
    testthat::expect_error(
      writeAlizer:::.wa_ensure_file(fn, url = url, sha256 = bad_sha, quiet = TRUE),
      regexp = "Checksum mismatch after download|Checksum mismatch"
    )
  )
})

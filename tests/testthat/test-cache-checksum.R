# tests/testthat/test-download-snapshots.R
test_that("offline-ish fetch error message is helpful", {
  # Keep cache isolated; not strictly required but nice for CI
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

  rel <- "models/x/y.bin"
  url <- "file:///nowhere.bin"  # guaranteed missing across OSes

  # We don't rely on the exact full sentence, just the key phrase.
  expect_error(
    writeAlizer:::.wa_ensure_file(rel, url),
    class  = "writeAlizer_download_missing",
    regexp = "Missing file for URL"
  )
})

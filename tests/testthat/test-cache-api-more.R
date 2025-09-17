# Extra coverage for cache.R without relying on interactive().

testthat::test_that("wa_cache_clear() reports when cache directory does not exist", {
  tmp_root <- withr::local_tempdir()
  # Point the cache root to a path that DOES NOT exist yet
  ghost <- file.path(tmp_root, "ghost-cache-root", "writeAlizer")
  testthat::expect_false(dir.exists(ghost))

  withr::local_options(writeAlizer.cache_dir = ghost)

  testthat::expect_message(
    ok <- writeAlizer::wa_cache_clear(ask = FALSE),
    "Cache directory does not exist"
  )
  testthat::expect_true(isTRUE(ok))
})

testthat::test_that("wa_cache_clear clears and recreates empty dir (no prompt)", {
  tmp_root <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = file.path(tmp_root, "R", "writeAlizer"))

  cdir <- writeAlizer::wa_cache_dir()
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)

  # Put a couple files and a subdir inside
  f1 <- file.path(cdir, "a.txt")
  f2 <- file.path(cdir, "b", "c.txt")
  dir.create(dirname(f2), recursive = TRUE, showWarnings = FALSE)
  writeLines("demo1", f1)
  writeLines("demo2", f2)
  testthat::expect_true(file.exists(f1) && file.exists(f2))

  # IMPORTANT: ask = FALSE so we never trigger a menu in interactive sessions.
  testthat::expect_message(
    ok <- writeAlizer::wa_cache_clear(ask = FALSE, preview = FALSE),
    "Cleared cache:"
  )
  testthat::expect_true(isTRUE(ok))

  # Directory is recreated and empty
  testthat::expect_true(dir.exists(cdir))
  left <- list.files(cdir, all.files = TRUE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  testthat::expect_length(left, 0L)
})

testthat::test_that("wa_cache_clear() is idempotent and keeps directory present", {
  tmp_root <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = file.path(tmp_root, "R", "writeAlizer"))

  cdir <- writeAlizer::wa_cache_dir()
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
  testthat::expect_true(dir.exists(cdir))

  # First clear (empty dir)
  testthat::expect_message(writeAlizer::wa_cache_clear(ask = FALSE), "Cleared cache:")
  testthat::expect_true(dir.exists(cdir))

  # Second clear (already empty); should still succeed and keep the directory
  testthat::expect_message(writeAlizer::wa_cache_clear(ask = FALSE), "Cleared cache:")
  testthat::expect_true(dir.exists(cdir))
})

testthat::test_that(".wa_cached_path() builds under wa_cache_dir()", {
  tmp_root <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = file.path(tmp_root, "R", "writeAlizer"))

  cdir <- writeAlizer::wa_cache_dir()
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)

  # Internal helper is used by artifact loader; touch a file via that path
  target <- writeAlizer:::.wa_cached_path("unit-test.bin")
  writeBin(charToRaw("ok"), target)
  testthat::expect_true(file.exists(target))

  # Compare prefixes using normalized forward slashes so it works on Windows too
  base_norm <- normalizePath(cdir, winslash = "/", mustWork = FALSE)
  targ_norm <- normalizePath(target, winslash = "/", mustWork = FALSE)
  testthat::expect_true(startsWith(targ_norm, paste0(base_norm, "/")))

  # Clear and ensure it’s gone (dir recreated empty)
  writeAlizer::wa_cache_clear(ask = FALSE)
  testthat::expect_true(dir.exists(cdir))
  testthat::expect_false(file.exists(target))
})

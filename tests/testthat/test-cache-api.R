testthat::test_that("wa_cache_dir()/wa_cache_clear() operate under an isolated cache root", {
  tmp_root <- withr::local_tempdir()
  # Make tools::R_user_dir() resolve under tmp_root for this test only:
  withr::local_envvar(R_USER_CACHE_DIR = tmp_root)

  cdir <- writeAlizer::wa_cache_dir()
  testthat::expect_true(is.character(cdir) && length(cdir) == 1L)

  # Ensure the cache dir exists before normalizing/inspecting
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)

  # Ensure the cache dir is under our isolated root
  testthat::expect_true(startsWith(
    normalizePath(cdir, winslash = "/", mustWork = FALSE),
    normalizePath(tmp_root, winslash = "/", mustWork = FALSE)
  ))

  # Create a fake file in the cache
  f <- file.path(cdir, "junk.txt")
  writeLines("junk", f)
  testthat::expect_true(file.exists(f))

  # Non-interactive removal
  ok <- writeAlizer::wa_cache_clear(ask = FALSE)
  testthat::expect_true(isTRUE(ok))

  # After clearing, the cache directory is recreated and empty
  testthat::expect_true(dir.exists(cdir))
  testthat::expect_length(list.files(cdir, all.files = TRUE, no.. = TRUE, recursive = TRUE), 0L)
})

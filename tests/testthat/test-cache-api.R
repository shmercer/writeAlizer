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

testthat::test_that("wa_cache_dir() honors writeAlizer.cache_dir override", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = tmp)

  cdir <- writeAlizer::wa_cache_dir()
  testthat::expect_identical(cdir, tmp)

  # You can write inside the overridden cache dir
  dir.create(cdir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(cdir, "hello.txt")
  writeLines("hi", f)
  testthat::expect_true(file.exists(f))
})

testthat::test_that(".wa_cached_path() composes paths under the cache dir", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = tmp)

  # Internal helper is fine to call in tests
  p <- writeAlizer:::.wa_cached_path("abc.rds")
  testthat::expect_identical(p, file.path(tmp, "abc.rds"))

  # Create the file and ensure it's where we expect
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(x = 1), p)
  testthat::expect_true(file.exists(p))
})

testthat::test_that("wa_cache_clear() on a non-existent cache dir returns TRUE and messages", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = file.path(tmp, "cache-root-does-not-exist"))

  # Directory does not exist yet
  testthat::expect_false(dir.exists(writeAlizer::wa_cache_dir()))

  # Should message that it doesn't exist and return TRUE (no-op)
  testthat::expect_message(
    res <- writeAlizer::wa_cache_clear(ask = FALSE),
    "Cache directory does not exist:",
    fixed = TRUE
  )
  testthat::expect_true(isTRUE(res))
})

testthat::test_that("wa_cache_clear() removes nested files and recreates empty cache dir", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = tmp)

  cdir <- writeAlizer::wa_cache_dir()
  dir.create(cdir, showWarnings = FALSE, recursive = TRUE)
  # Create a few nested files and a subdir
  writeLines("a", file.path(cdir, "a.txt"))
  dir.create(file.path(cdir, "sub"), showWarnings = FALSE)
  writeLines("b", file.path(cdir, "sub", "b.txt"))
  testthat::expect_true(length(list.files(cdir, recursive = TRUE)) >= 2L)

  # Clear (non-interactively) – should delete then recreate the empty cache dir
  testthat::expect_message(
    ok <- writeAlizer::wa_cache_clear(ask = FALSE),
    "Cleared cache:",
    fixed = TRUE
  )
  testthat::expect_true(isTRUE(ok))
  testthat::expect_true(dir.exists(cdir))
  testthat::expect_identical(length(list.files(cdir, recursive = TRUE)), 0L)
})


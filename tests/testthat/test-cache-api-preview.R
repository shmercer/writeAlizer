# tests/testthat/test-cache-api-preview.R

testthat::test_that("wa_cache_clear() preview shows items and sizes (interactive, Yes)", {
  tmp_root <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = file.path(tmp_root, "R", "writeAlizer"))

  cdir <- writeAlizer::wa_cache_dir()
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
  writeLines("abc", file.path(cdir, "one.txt"))
  dir.create(file.path(cdir, "subdir"), recursive = TRUE, showWarnings = FALSE)
  writeLines("abcdefg", file.path(cdir, "subdir", "two.bin"))

  # force interactive + choose "Yes"
  withr::local_options(
    writeAlizer.force_interactive = TRUE,
    writeAlizer.menu_fn = function(choices, title = NULL) 2L  # 2 = Yes
  )

  msgs <- testthat::capture_messages({
    ok <- writeAlizer::wa_cache_clear(ask = TRUE, preview = TRUE)
    testthat::expect_true(isTRUE(ok))
  })

  testthat::expect_true(any(grepl("^About to delete \\d+ item", msgs)))
  testthat::expect_true(any(grepl("^Preview \\(first", msgs)))
  testthat::expect_true(any(grepl("^Cleared cache:", msgs)))

  # cache re-created and empty
  testthat::expect_true(dir.exists(cdir))
  left <- list.files(cdir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  testthat::expect_length(left, 0L)
})

testthat::test_that("wa_cache_clear() preview with 'No' leaves cache intact", {
  tmp_root <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = file.path(tmp_root, "R", "writeAlizer"))

  cdir <- writeAlizer::wa_cache_dir()
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
  marker <- file.path(cdir, "keep.me")
  writeLines("keep", marker)

  withr::local_options(
    writeAlizer.force_interactive = TRUE,
    writeAlizer.menu_fn = function(choices, title = NULL) 1L  # 1 = No
  )

  msgs <- testthat::capture_messages({
    ok <- writeAlizer::wa_cache_clear(ask = TRUE, preview = TRUE)
    testthat::expect_false(isTRUE(ok))
  })

  testthat::expect_true(any(grepl("^About to delete \\d+ item", msgs)))
  testthat::expect_false(any(grepl("^Cleared cache:", msgs)))
  testthat::expect_true(file.exists(marker))
})

testthat::test_that("wa_cache_clear() reports a failure when unlink returns non-zero", {
  tmp_root <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = file.path(tmp_root, "R", "writeAlizer"))

  cdir <- writeAlizer::wa_cache_dir()
  dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(cdir, "blocked.txt"))

  # Make unlink fail
  withr::local_options(writeAlizer.unlink_fn = function(...) 1L)

  warns <- testthat::capture_warnings({
    ok <- writeAlizer::wa_cache_clear(ask = FALSE)
    testthat::expect_false(isTRUE(ok))
  })

  testthat::expect_true(any(grepl("^Failed to clear:", warns)))
  testthat::expect_true(dir.exists(cdir))
})

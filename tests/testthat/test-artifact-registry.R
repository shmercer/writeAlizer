testthat::test_that(".wa_canonical_model maps rb_mod3* to *_v2", {
  canon <- getFromNamespace(".wa_canonical_model", "writeAlizer")
  testthat::expect_equal(canon("rb_mod3all"), "rb_mod3all_v2")
  testthat::expect_equal(canon("rb_mod3narr"), "rb_mod3narr_v2")
  testthat::expect_equal(canon("coh_mod3all"), "coh_mod3all")  # unchanged
})

testthat::test_that(".wa_parts_for returns entries for known keys", {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")
  # RDA parts should exist for rb_mod1 and others in the static registry
  rda_rb1 <- parts_for("rda", "rb_mod1")
  testthat::expect_gt(nrow(rda_rb1), 0L)

  # RDS varlists should exist for a mod3 family model
  rds_rb3 <- parts_for("rds", "rb_mod3all")
  testthat::expect_gt(nrow(rds_rb3), 0L)
})

test_that(".wa_ensure_file fetches from a local file:// URL (no network)", {
  tmp <- withr::local_tempdir()
  # Isolate writeAlizer cache so we don't touch user cache
  withr::local_envvar(R_USER_CACHE_DIR = file.path(tmp, "R-cache"))

  # Create a tiny local source file and expose as file:// URL
  src <- file.path(tmp, "src.bin")
  writeBin(as.raw(1:10), src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  # Pick a cache filename and ensure it downloads into the isolated cache
  fn <- "unit_test_file.bin"
  p <- writeAlizer:::.wa_ensure_file(fn, url = url, quiet = TRUE)
  expect_true(file.exists(p))

  # Re-ensure should be a cache hit; still exists and same path
  p2 <- writeAlizer:::.wa_ensure_file(fn, url = url, quiet = TRUE)
  expect_identical(p, p2)
})


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

testthat::test_that(".wa_ensure_file uses writeAlizer.mock_dir override (no network)", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")

  tmp <- withr::local_tempdir()
  withr::local_options(list(writeAlizer.mock_dir = tmp))

  fake_name <- "mock_artifact.rda"
  fake_path <- file.path(tmp, fake_name)
  save(list = character(), file = fake_path)  # empty .rda is fine for path check

  # URL is ignored when override hits; use a placeholder
  out <- ensure_file(fake_name, url = "https://example.invalid/file")
  testthat::expect_identical(normalizePath(out), normalizePath(fake_path))
})

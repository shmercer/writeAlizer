testthat::test_that("keep_stem_before_txt handles mixed paths and extensions", {
  x <- c(
    "C:/data/3401.txt",
    "E:\\\\samples\\\\1002.TXT",
    "no_extension_id",
    "just.txt",       # becomes "just"
    "/a/b/essay_v2.docx",  # unchanged (no .txt at end)
    NA
  )
  out <- keep_stem_before_txt(x)

  testthat::expect_identical(
    out,
    c("3401", "1002", "no_extension_id", "just", "essay_v2.docx", NA_character_)
  )
})

testthat::test_that("keep_stem_before_txt preserves NA and returns character", {
  x <- c(NA, "3401.txt", "plain")
  out <- keep_stem_before_txt(x)

  testthat::expect_true(is.character(out))
  testthat::expect_true(is.na(out[1L]))
  testthat::expect_identical(out[2L], "3401")
  testthat::expect_identical(out[3L], "plain")
})

testthat::test_that("keep_stem_before_txt is idempotent on already-trimmed values", {
  x <- c("3401", "alpha_beta")
  out1 <- keep_stem_before_txt(x)
  out2 <- keep_stem_before_txt(out1)
  testthat::expect_identical(out1, out2)
})

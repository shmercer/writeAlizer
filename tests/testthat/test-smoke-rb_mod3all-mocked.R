test_that("preprocess rb_mod3all works offline via mocked varlists", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  skip_if(!file.exists(rb_path), "sample_rb.csv not installed")

  rb <- import_rb(rb_path)
  nms <- setdiff(names(rb), "ID")
  skip_if(length(nms) < 9, "Not enough feature columns in sample_rb.csv")

  # Mock the exact varlist filenames the code expects (no downloads)
  tmp <- tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)
  saveRDS(nms[1:3], file.path(tmp, "rb_exp_vars_v2.rds"))
  saveRDS(nms[4:6], file.path(tmp, "rb_narr_vars_v2.rds"))
  saveRDS(nms[7:9], file.path(tmp, "rb_per_vars_v2.rds"))

  # Suppress caret's zero-variance messages
  res <- suppressWarnings(preprocess(rb, model = "rb_mod3all"))

  expect_true(is.list(res) && length(res) == 3)
  lapply(res, function(df) {
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), nrow(rb))
  })
})

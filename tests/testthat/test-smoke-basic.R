test_that("import + preprocess (rb_mod1) runs offline quickly", {
  rb_path <- wa_sample_path("sample_rb.csv")
  testthat::skip_if(!nzchar(rb_path)  || !file.exists(rb_path),  "sample_rb.csv not found")

  rb <- import_rb(rb_path)
  expect_gt(nrow(rb), 0)

  # rb_mod1: allow for list return; suppress benign zero-variance warnings
  res <- suppressWarnings(preprocess(rb, model = "rb_mod1"))

  if (is.data.frame(res)) {
    expect_equal(nrow(res), nrow(rb))
  } else if (is.list(res) && length(res) > 0) {
    lapply(res, function(df) {
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), nrow(rb))
    })
  } else {
    fail(sprintf("Unexpected preprocess() return type: %s", paste(class(res), collapse = "/")))
  }
})

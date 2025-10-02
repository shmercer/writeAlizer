testthat::test_that("preprocess rb_mod3all works offline via mocked varlists", {
  rb_path <- wa_sample_path("sample_rb.csv")
  testthat::skip_if(!nzchar(rb_path) || !file.exists(rb_path), "sample_rb.csv not found")
  rb <- writeAlizer::import_rb(rb_path)

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp, writeAlizer.offline = TRUE)

  all_cols <- setdiff(colnames(rb), "ID")
  testthat::skip_if(length(all_cols) < 15, "Not enough columns in sample RB file")

  # Minimal registry rows (rds only) for rb_mod3all_v2
  reg <- data.frame(
    kind  = rep("rds", 3),
    model = rep("rb_mod3all_v2", 3),
    part  = c("a","b","c"),
    file  = c("rb_exp_vars_v2.rds", "rb_narr_vars_v2.rds", "rb_per_vars_v2.rds"),
    url   = file.path("file://", normalizePath(tmp, winslash = "/", mustWork = TRUE),
                      c("rb_exp_vars_v2.rds", "rb_narr_vars_v2.rds", "rb_per_vars_v2.rds")),
    sha   = NA_character_,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() reg)

  vlist <- list(all_cols[1:5], all_cols[6:10], all_cols[11:15])
  saveRDS(vlist[[1]], file.path(tmp, "rb_exp_vars_v2.rds"))
  saveRDS(vlist[[2]], file.path(tmp, "rb_narr_vars_v2.rds"))
  saveRDS(vlist[[3]], file.path(tmp, "rb_per_vars_v2.rds"))

  res <- writeAlizer:::preprocess("rb_mod3all", rb)

  # Allow consolidation: require between 1 and 3 splits
  testthat::expect_true(is.list(res) && length(res) >= 1L && length(res) <= 3L)

  # Basic shape checks
  for (df in res) {
    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_true("ID" %in% names(df))
  }

  # Each varlist should be fully present in at least one split
  has_set <- function(set) any(vapply(res, function(df) all(set %in% names(df)), logical(1)))
  testthat::expect_true(has_set(vlist[[1]]))
  testthat::expect_true(has_set(vlist[[2]]))
  testthat::expect_true(has_set(vlist[[3]]))
})

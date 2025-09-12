testthat::test_that("preprocess rb_mod3all works offline via mocked varlists", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(rb_path != "", "sample_rb.csv not found in inst/extdata")
  rb <- writeAlizer::import_rb(rb_path)

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  all_cols <- setdiff(colnames(rb), "ID")
  testthat::skip_if(length(all_cols) < 15, "Not enough columns in sample RB file")

  canon <- getFromNamespace(".wa_canonical_model", "writeAlizer")
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")

  model <- canon("rb_mod3all")  # canonical key (may be *_v2, etc.)
  parts <- parts_for("rds", model)
  testthat::skip_if(nrow(parts) < 1, sprintf("No RDS parts registered for %s", model))

  # Detect the column with .rds varlist paths; drop NAs/empties
  col_has_rds <- vapply(
    parts,
    function(x) is.character(x) && any(grepl("\\.rds$", x, ignore.case = TRUE), na.rm = TRUE),
    logical(1)
  )
  testthat::skip_if(!any(col_has_rds), "Registry has no .rds filename column")
  files <- unname(parts[[names(parts)[which(col_has_rds)[1]]]])
  files <- files[!is.na(files) & nzchar(files) & grepl("\\.rds$", files, ignore.case = TRUE)]
  testthat::skip_if(length(files) == 0, "No .rds filenames resolved from registry")

  # Build up to three non-overlapping varlists from available columns
  vlist <- list(all_cols[1:5], all_cols[6:10], all_cols[11:15])
  n_make <- min(3L, length(files), length(vlist))

  mk_rds <- function(relpath, vec) {
    full <- file.path(tmp, relpath)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    saveRDS(vec, full)
    TRUE
  }
  for (i in seq_len(n_make)) mk_rds(files[i], vlist[[i]])

  res <- writeAlizer:::preprocess(model, rb)

  # Some registries may consolidate splits; require at least one and at most what we created
  testthat::expect_true(is.list(res) && length(res) >= 1 && length(res) <= n_make)
  for (df in res) {
    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_true("ID" %in% names(df))
  }
})

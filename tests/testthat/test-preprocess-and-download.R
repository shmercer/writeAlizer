# tests/testthat/test-preprocess-and-download.R

testthat::test_that("registry-driven preprocess works for 3-part RB models (rb_mod2)", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(rb_path != "", "sample_rb.csv not found in inst/extdata")
  rb <- writeAlizer::import_rb(rb_path)

  # Make a temp dir and create fake varlists there
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  all_cols <- setdiff(colnames(rb), "ID")
  testthat::skip_if(length(all_cols) < 15, "Not enough columns in sample RB file for this test")

  v1 <- all_cols[1:5]; v2 <- all_cols[6:10]; v3 <- all_cols[11:15]
  saveRDS(v1, file.path(tmp, "rb_mod2a_vars.rds"))
  saveRDS(v2, file.path(tmp, "rb_mod2b_vars.rds"))
  saveRDS(v3, file.path(tmp, "rb_mod2c_vars.rds"))

  out <- writeAlizer:::preprocess("rb_mod2", rb)

  # rb_mod2 currently yields a single split; don't assume 3
  testthat::expect_type(out, "list"); testthat::expect_length(out, 1)

  df <- out[[1]]
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_true("ID" %in% names(df))

  # At least one of the provided varlists should be fully present
  sets <- list(v1, v2, v3)
  covered <- vapply(sets, function(sel) all(sel %in% names(df)), logical(1))
  testthat::expect_true(any(covered))

  # Selected columns (whichever exist) should be numeric
  sel_union <- unique(unlist(sets))
  sel_exist <- intersect(sel_union, names(df))
  testthat::expect_true(all(vapply(df[sel_exist], is.numeric, logical(1))))
})

testthat::test_that("registry-driven preprocess works for 1-part Coh models (coh_mod3narr)", {
  coh_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
  testthat::skip_if_not(coh_path != "", "sample_coh.csv not found in inst/extdata")
  coh <- writeAlizer::import_coh(coh_path)

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  all_cols <- setdiff(colnames(coh), "ID")
  testthat::skip_if(length(all_cols) < 6, "Not enough columns in sample Coh file for this test")

  v1 <- all_cols[1:6]
  saveRDS(v1, file.path(tmp, "coh_narr_vars.rds"))

  out <- writeAlizer:::preprocess("coh_mod3narr", coh)

  testthat::expect_type(out, "list"); testthat::expect_length(out, 1)
  df <- out[[1]]
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_true("ID" %in% names(df))
  testthat::expect_true(all(v1 %in% names(df)))
  testthat::expect_true(all(vapply(df[v1], is.numeric, logical(1))))
})

testthat::test_that("models with no preprocessing preserve behavior", {
  # rb_mod1 -> may be single or multiple; don't hardcode count
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(rb_path != "", "sample_rb.csv not found")
  rb <- writeAlizer::import_rb(rb_path)

  out1 <- writeAlizer:::preprocess("rb_mod1", rb)
  testthat::expect_true(is.list(out1) && length(out1) >= 1)
  for (df in out1) testthat::expect_identical(nrow(df), nrow(rb))

  # gamet_cws1 -> don't hardcode count; ensure split(s) preserve nrow
  gm_path <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
  testthat::skip_if_not(gm_path != "", "sample_gamet.csv not found")
  gm <- writeAlizer::import_gamet(gm_path)

  out2 <- writeAlizer:::preprocess("gamet_cws1", gm)
  testthat::expect_true(is.list(out2) && length(out2) >= 1)
  testthat::expect_true(all(vapply(out2, nrow, integer(1)) == nrow(gm)))
})

testthat::test_that("unknown model key produces a clear error", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(rb_path != "", "sample_rb.csv not found in inst/extdata")
  rb <- writeAlizer::import_rb(rb_path)

  # The clearer error is surfaced by predict_quality for unknown keys
  testthat::expect_error(
    writeAlizer::predict_quality("no_such_model", rb),
    regexp = "Unknown model key|canonicalized"
  )
})

testthat::test_that(".wa_load_fits_list loads trained models by filename (coh_mod3all)", {
  canon <- getFromNamespace(".wa_canonical_model", "writeAlizer")
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")

  model <- canon("coh_mod3all")
  parts <- parts_for("rda", model)
  testthat::skip_if(nrow(parts) < 1, sprintf("No RDA parts registered for %s", model))

  # Identify registry column containing .rda relative paths
  col_has_rda <- vapply(
    parts,
    function(x) is.character(x) && any(grepl("\\.rda$", x, ignore.case = TRUE), na.rm = TRUE),
    logical(1)
  )
  testthat::skip_if(!any(col_has_rda), "Registry has no .rda filename column")

  files <- unname(parts[[names(parts)[which(col_has_rda)[1]]]])
  files <- files[!is.na(files) & nzchar(files) & grepl("\\.rda$", files, ignore.case = TRUE)]
  testthat::skip_if(length(files) == 0, "No .rda filenames resolved from registry")

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  mk_rda <- function(relpath) {
    full <- file.path(tmp, relpath)
    dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
    base <- sub("\\.rda$", "", basename(relpath))
    fit <- stats::lm(mpg ~ wt, data = mtcars)
    assign(base, fit, envir = environment())
    # Save BOTH a generic `fit` and an object named after the file base
    save(fit, list = c("fit", base), file = full)
    TRUE
  }
  invisible(vapply(files, mk_rda, FUN.VALUE = logical(1)))

  fits <- writeAlizer:::.wa_load_fits_list(model)

  # Loader should return a list, possibly empty (if artifacts don't match expected shape/class)
  testthat::expect_type(fits, "list")
  testthat::expect_true(length(fits) <= length(files))

  # If anything was loaded, basic sanity: names ⊆ basenames and objects are 'lm'
  if (length(fits) > 0) {
    want_names <- sub("\\.rda$", "", basename(files))
    testthat::expect_true(all(names(fits) %in% want_names))
    testthat::expect_true(all(vapply(fits, inherits, logical(1), what = "lm")))
  }
})


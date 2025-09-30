withr::local_options(writeAlizer.mock_dir = {
  old <- getOption("writeAlizer.mock_dir")
  ex  <- tryCatch(writeAlizer::wa_seed_example_models("example", dir = tempdir()),
                  error = function(e) NULL)
  if (!is.null(ex)) ex else old
})

testthat::test_that("predict_quality runs for all model keys with mocked artifacts", {
  # Keep tests offline
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  # Sample data included with the package
  rb_path  <- system.file("extdata", "sample_rb.csv",    package = "writeAlizer")
  coh_path <- system.file("extdata", "sample_coh.csv",   package = "writeAlizer")
  gam_path <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
  testthat::skip_if(rb_path  == "", "RB sample missing")
  testthat::skip_if(coh_path == "", "Coh sample missing")
  testthat::skip_if(gam_path == "", "GAMET sample missing")

  rb  <- writeAlizer::import_rb(rb_path)
  coh <- writeAlizer::import_coh(coh_path)
  gam <- writeAlizer::import_gamet(gam_path)

  # Model families
  models_rb <- c("rb_mod1","rb_mod2","rb_mod3narr","rb_mod3exp","rb_mod3per",
                 "rb_mod3all","rb_mod3narr_v2","rb_mod3exp_v2","rb_mod3per_v2",
                 "rb_mod3all_v2")
  models_coh <- c("coh_mod1","coh_mod2","coh_mod3narr","coh_mod3exp","coh_mod3per","coh_mod3all")
  models_gam <- c("gamet_cws1")

  # Helpers ---------------------------------------------------------------

  # numeric feature names (drop ID)
  num_cols <- function(df) setdiff(names(df)[vapply(df, is.numeric, logical(1))], "ID")

  # split a vector of columns into n parts of size k (contiguous)
  pick_vars <- function(df, n_parts, per_part = 5L) {
    cols <- num_cols(df)
    testthat::skip_if(length(cols) < n_parts * per_part, "Not enough numeric columns for mocked varlists")
    out <- vector("list", n_parts)
    idx <- 1L
    for (i in seq_len(n_parts)) {
      out[[i]] <- cols[idx:(idx + per_part - 1L)]
      idx <- idx + per_part
    }
    out
  }

  # Train a small lm fit from a preprocessed sub-data frame (object inside is 'fit')
  mk_fit_rda_from_pp <- function(subdf, outfile) {
    drop_id <- function(df) if ("ID" %in% names(df)) df[setdiff(names(df), "ID")] else df
    x <- drop_id(subdf)
    testthat::skip_if(ncol(x) < 1L, "Preprocessed part has no predictors")

    v <- names(x)[1L]
    tr <- data.frame(y = x[[v]], check.names = FALSE)
    tr[[v]] <- x[[v]]
    fit <- stats::lm(y ~ ., data = tr)
    save(fit, file = outfile)
  }

  run_for_model <- function(model, data) {
    # 1) Mock varlists if this model uses them
    rds_parts <- writeAlizer:::.wa_parts_for("rds", model)
    if (nrow(rds_parts) > 0L) {
      vars_slices <- pick_vars(data, nrow(rds_parts), per_part = 5L)
      for (i in seq_len(nrow(rds_parts))) {
        saveRDS(vars_slices[[i]], file.path(tmp, rds_parts$file[i]))
      }
    } else if (model == "gamet_cws1") {
      # preprocess() returns two copies; no varlists needed
    }

    # 2) Preprocess now that varlists (if any) are in place
    pp <- suppressWarnings(writeAlizer:::preprocess(model, data))

    # 3) Mock trained fits for each .rda in the registry (from the preprocessed parts)
    rda_parts <- writeAlizer:::.wa_parts_for("rda", model)
    testthat::expect_gt(nrow(rda_parts), 0L)
    testthat::expect_equal(length(pp), nrow(rda_parts))

    # outward (display) names: strip "_v2" for rb_mod3 family
    base_names <- tools::file_path_sans_ext(basename(rda_parts$file))
    display_names <- if (grepl("^rb_mod3", writeAlizer:::.wa_canonical_model(model))) {
      sub("_v2$", "", base_names)
    } else base_names
    expected_preds <- paste0("pred_", display_names)

    # mean column name (never include _v2)
    mean_name <- paste0("pred_", sub("_v2$", "", model), "_mean")

    for (i in seq_len(nrow(rda_parts))) {
      mk_fit_rda_from_pp(pp[[i]], file.path(tmp, rda_parts$file[i]))
    }

    # 4) Predict via package API
    out <- suppressWarnings(writeAlizer::predict_quality(model, data))

    # 5) Validate output columns and basic shapes (use display names)
    testthat::expect_true(all(expected_preds %in% names(out)))
    testthat::expect_identical(out$ID, data$ID)

    # mean column presence/absence
    if (model == "gamet_cws1") {
      testthat::expect_false(mean_name %in% names(out))  # special rule
    } else if (length(expected_preds) > 1L) {
      testthat::expect_true(mean_name %in% names(out))
    }

    # Predictions are numeric of correct length
    for (nm in expected_preds) {
      testthat::expect_true(is.numeric(out[[nm]]))
      testthat::expect_identical(length(out[[nm]]), nrow(data))
    }

    invisible(out)
  }

  invisible(lapply(models_rb,  run_for_model, data = rb))
  invisible(lapply(models_coh, run_for_model, data = coh))
  invisible(lapply(models_gam, run_for_model, data = gam))
})

test_that("predict_quality('example') runs offline quickly", {
  coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)
  writeAlizer:::wa_seed_example_models("example", dir = tmp)

  out <- suppressWarnings(predict_quality("example", coh))
  expect_s3_class(out, "data.frame")
  expect_true(all(c("ID", "pred_example") %in% names(out)))
})

test_that("preprocess rb_mod3all works offline via mocked varlists", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  skip_if(!file.exists(rb_path), "sample_rb.csv not installed")

  rb <- import_rb(rb_path)
  nms <- setdiff(names(rb), "ID")
  skip_if(length(nms) < 9, "Not enough feature columns in sample_rb.csv")

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)
  saveRDS(nms[1:3], file.path(tmp, "rb_exp_vars_v2.rds"))
  saveRDS(nms[4:6], file.path(tmp, "rb_narr_vars_v2.rds"))
  saveRDS(nms[7:9], file.path(tmp, "rb_per_vars_v2.rds"))

  out <- suppressWarnings(preprocess("rb_mod3all", rb))
  expect_true(is.list(out) && length(out) == 3L)
})

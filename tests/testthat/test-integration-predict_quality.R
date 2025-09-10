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

  # Save a small lm fit under a given .rda filename (object inside is 'fit')
  mk_fit_rda <- function(df, vars, outfile) {
    # Use exactly one predictor to avoid rank deficiency with tiny samples
    v <- vars[[1L]]

    # Build training data with the original predictor name preserved
    tr <- data.frame(y = df[[v]], check.names = FALSE)
    tr[[v]] <- df[[v]]

    # Fit using '.' so we don't reference the name explicitly
    fit <- stats::lm(y ~ ., data = tr)

    save(fit, file = outfile)
  }

  run_for_model <- function(model, data) {
    # 1) Mock varlists if this model uses them
    rds_parts <- writeAlizer:::.wa_parts_for("rds", model)
    vars_slices <- list()
    if (nrow(rds_parts) > 0L) {
      vars_slices <- pick_vars(data, nrow(rds_parts), per_part = 5L)
      for (i in seq_len(nrow(rds_parts))) {
        saveRDS(vars_slices[[i]], file.path(tmp, rds_parts$file[i]))
      }
    } else if (model == "gamet_cws1") {
      # preprocess() returns two copies; no varlists needed
    }

    # 2) Mock trained fits for each .rda in the registry
    rda_parts <- writeAlizer:::.wa_parts_for("rda", model)
    testthat::expect_gt(nrow(rda_parts), 0L)

    # outward (display) names: strip "_v2" for rb_mod3 family
    base_names <- tools::file_path_sans_ext(basename(rda_parts$file))
    display_names <- if (grepl("^rb_mod3", writeAlizer:::.wa_canonical_model(model))) {
      sub("_v2$", "", base_names)
    } else base_names
    expected_preds <- paste0("pred_", display_names)

    # mean column name (never include _v2)
    mean_name <- paste0("pred_", sub("_v2$", "", model), "_mean")

    # Choose vars per part: use the vars slice when available; otherwise fall back
    fallback <- head(num_cols(data), 5L)
    testthat::skip_if(length(fallback) < 1L, "No numeric columns available for fallback")

    for (i in seq_len(nrow(rda_parts))) {
      vars <- if (length(vars_slices) >= i) vars_slices[[i]] else fallback
      mk_fit_rda(data, vars, file.path(tmp, rda_parts$file[i]))
    }

    # 3) Predict via package API
    out <- writeAlizer:::predict_quality(model, data)

    # 4) Validate output columns and basic shapes (use display names)
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

withr::local_options(writeAlizer.mock_dir = {
  old <- getOption("writeAlizer.mock_dir")
  ex  <- tryCatch(writeAlizer::wa_seed_example_models("example", dir = tempdir()),
                  error = function(e) NULL)
  if (!is.null(ex)) ex else old
})

withr::local_envvar(R_USER_CACHE_DIR = tempdir())

testthat::test_that("predict_quality runs for all model keys with mocked artifacts", {
  # Keep tests offline & use a temp mock dir
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)
  withr::local_options(writeAlizer.offline = TRUE)

  # Sample data (use robust helper)
  rb_path  <- wa_sample_path("sample_rb.csv")
  coh_path <- wa_sample_path("sample_coh.csv")
  gam_path <- wa_sample_path("sample_gamet.csv")
  testthat::skip_if(!nzchar(rb_path)  || !file.exists(rb_path),  "sample_rb.csv not found")
  testthat::skip_if(!nzchar(coh_path) || !file.exists(coh_path), "sample_coh.csv not found")
  testthat::skip_if(!nzchar(gam_path) || !file.exists(gam_path), "sample_gamet.csv not found")

  rb  <- writeAlizer::import_rb(rb_path)
  coh <- writeAlizer::import_coh(coh_path)
  gam <- writeAlizer::import_gamet(gam_path)

  # Model families
  models_rb  <- c("rb_mod1","rb_mod2","rb_mod3narr","rb_mod3exp","rb_mod3per",
                  "rb_mod3all","rb_mod3narr_v2","rb_mod3exp_v2","rb_mod3per_v2","rb_mod3all_v2")
  models_coh <- c("coh_mod1","coh_mod2","coh_mod3narr","coh_mod3exp","coh_mod3per","coh_mod3all")
  models_gam <- c("gamet_cws1")

  # -------- Mock registry covering all models used in this test --------
  make_rows <- function(kind, model, files) {
    data.frame(
      kind  = kind,
      model = model,
      part  = seq_along(files),
      file  = files,
      url   = file.path("file://", normalizePath(tmp, winslash = "/", mustWork = TRUE), files),
      sha   = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  reg_rds <- rbind(
    # ---- RB v1/v2 varlists (ADD THESE TWO LINES) ----
    make_rows("rds", "rb_mod1",
              paste0("rb_mod1", letters[1:6], "_vars.rds")),
    make_rows("rds", "rb_mod2",
              paste0("rb_mod2", letters[1:3], "_vars.rds")),

    # ---- RB v2 varlists (already present) ----
    make_rows("rds", "rb_mod3all_v2",
              c("rb_exp_vars_v2.rds", "rb_narr_vars_v2.rds", "rb_per_vars_v2.rds")),
    make_rows("rds", "rb_mod3narr_v2", "rb_narr_vars_v2.rds"),
    make_rows("rds", "rb_mod3exp_v2",  "rb_exp_vars_v2.rds"),
    make_rows("rds", "rb_mod3per_v2",  "rb_per_vars_v2.rds"),

    # ---- Coh-Metrix varlists (mod1 + mod2 + mod3) ----
    make_rows("rds", "coh_mod1",
              paste0("coh_mod1", letters[1:6], "_vars.rds")),
    make_rows("rds", "coh_mod2",
              paste0("coh_mod2", letters[1:3], "_vars.rds")),
    make_rows("rds", "coh_mod3all",
              c("coh_exp_vars.rds", "coh_narr_vars.rds", "coh_per_vars.rds")),
    make_rows("rds", "coh_mod3narr", "coh_narr_vars.rds"),
    make_rows("rds", "coh_mod3exp",  "coh_exp_vars.rds"),
    make_rows("rds", "coh_mod3per",  "coh_per_vars.rds")
  )

  reg_rda <- rbind(
    # rb_mod1: six submodels
    make_rows("rda", "rb_mod1", paste0("rb_mod1", letters[1:6], ".rda")),
    # rb_mod2: three submodels
    make_rows("rda", "rb_mod2", paste0("rb_mod2", letters[1:3], ".rda")),
    # rb_mod3 v2: three singletons + the all v2 triad
    make_rows("rda", "rb_mod3narr_v2", "rb_mod3narr_v2.rda"),
    make_rows("rda", "rb_mod3exp_v2",  "rb_mod3exp_v2.rda"),
    make_rows("rda", "rb_mod3per_v2",  "rb_mod3per_v2.rda"),
    make_rows("rda", "rb_mod3all_v2",  c("rb_mod3exp_v2.rda","rb_mod3narr_v2.rda","rb_mod3per_v2.rda")),

    # Coh: mod1 (6), mod2 (3), mod3 singles + all (3)
    make_rows("rda", "coh_mod1",     paste0("coh_mod1", letters[1:6], ".rda")),
    make_rows("rda", "coh_mod2",     paste0("coh_mod2", letters[1:3], ".rda")),
    make_rows("rda", "coh_mod3narr", "coh_mod3narr.rda"),
    make_rows("rda", "coh_mod3exp",  "coh_mod3exp.rda"),
    make_rows("rda", "coh_mod3per",  "coh_mod3per.rda"),
    make_rows("rda", "coh_mod3all",  c("coh_mod3exp.rda","coh_mod3narr.rda","coh_mod3per.rda")),

    # GAMET: two fits
    make_rows("rda", "gamet_cws1",   c("CWS_mod1a.rda","CIWS_mod1a.rda")),

    # example model (used elsewhere, harmless here)
    make_rows("rda", "example",      "example.rda")
  )

  testthat::local_mocked_bindings(
    .package = "writeAlizer",
    .wa_registry = function() rbind(reg_rds, reg_rda)
  )
  # ---------------------------------------------------------------------

  # Helpers -------------------------------------------------------------
  num_cols <- function(df) setdiff(names(df)[vapply(df, is.numeric, logical(1))], "ID")

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

  mk_fit_rda_from_pp <- function(subdf, outfile) {
    drop_id <- function(dd) if ("ID" %in% names(dd)) dd[setdiff(names(dd), "ID")] else dd
    x <- drop_id(subdf); testthat::skip_if(ncol(x) < 1L, "Preprocessed part has no predictors")
    v <- names(x)[1L]
    tr <- data.frame(y = x[[v]], check.names = FALSE); tr[[v]] <- x[[v]]
    fit <- stats::lm(y ~ ., data = tr)
    save(fit, file = file.path(tmp, basename(outfile))) # save under mock_dir basename
  }

  run_for_model <- function(model, data) {
    # 1) Mock varlists if needed
    rds_parts <- writeAlizer:::.wa_parts_for("rds", model)
    if (nrow(rds_parts) > 0L) {
      vars_slices <- pick_vars(data, nrow(rds_parts), per_part = 5L)
      for (i in seq_len(nrow(rds_parts))) {
        saveRDS(vars_slices[[i]], file.path(tmp, basename(rds_parts$file[i])))
      }
    }

    # 2) Preprocess now
    pp <- suppressWarnings(writeAlizer:::preprocess(model, data))

    # 3) Create fits corresponding to registry RDA rows
    rda_parts <- writeAlizer:::.wa_parts_for("rda", model)
    testthat::expect_gt(nrow(rda_parts), 0L)
    testthat::expect_equal(length(pp), nrow(rda_parts))
    for (i in seq_len(nrow(rda_parts))) {
      mk_fit_rda_from_pp(pp[[i]], rda_parts$file[i])
    }

    # Display names & expected columns
    base_names    <- tools::file_path_sans_ext(basename(rda_parts$file))
    display_names <- if (grepl("^rb_mod3", writeAlizer:::.wa_canonical_model(model))) sub("_v2$", "", base_names) else base_names
    expected_preds <- paste0("pred_", display_names)
    mean_name <- paste0("pred_", sub("_v2$", "", model), "_mean")

    # 4) Predict via API
    out <- suppressWarnings(writeAlizer::predict_quality(model, data))

    # 5) Validate
    testthat::expect_true(all(expected_preds %in% names(out)))
    testthat::expect_identical(out$ID, data$ID)

    if (model == "gamet_cws1") {
      testthat::expect_false(mean_name %in% names(out))
    } else if (length(expected_preds) > 1L) {
      testthat::expect_true(mean_name %in% names(out))
    }

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
  coh_path <- wa_sample_path("sample_coh.csv")
  if (!nzchar(coh_path) || !file.exists(coh_path)) {
    testthat::skip_if(!nzchar(coh_path) || !file.exists(coh_path), "sample_coh.csv not found")
  }
  coh <- import_coh(wa_sample_path("sample_coh.csv"))
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)
  writeAlizer::wa_seed_example_models("example", dir = tmp)

  out <- suppressWarnings(predict_quality("example", coh))
  expect_s3_class(out, "data.frame")
  expect_true(all(c("ID", "pred_example") %in% names(out)))
})

test_that("preprocess rb_mod3all works offline via mocked varlists", {
  withr::local_options(writeAlizer.offline = TRUE)

  rb_path <- wa_sample_path("sample_rb.csv")
  testthat::skip_if(!nzchar(rb_path) || !file.exists(rb_path), "sample_rb.csv not found")
  rb <- import_rb(rb_path)
  nms <- setdiff(names(rb), "ID")
  skip_if(length(nms) < 9, "Not enough feature columns in sample_rb.csv")

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  # Mock registry (only rds rows needed here)
  make_rows <- function(files) {
    data.frame(
      kind  = "rds",
      model = "rb_mod3all_v2",
      part  = seq_along(files),
      file  = files,
      url   = file.path("file://", normalizePath(tmp, winslash = "/", mustWork = TRUE), files),
      sha   = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  reg <- make_rows(c("rb_exp_vars_v2.rds", "rb_narr_vars_v2.rds", "rb_per_vars_v2.rds"))
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() reg)

  # Provide the mocked varlists
  saveRDS(nms[1:3], file.path(tmp, "rb_exp_vars_v2.rds"))
  saveRDS(nms[4:6], file.path(tmp, "rb_narr_vars_v2.rds"))
  saveRDS(nms[7:9], file.path(tmp, "rb_per_vars_v2.rds"))

  out <- suppressWarnings(preprocess("rb_mod3all", rb))
  expect_true(is.list(out) && length(out) == 3L)
})

test_that("predict_quality coerces 1-col df/matrix predictions to vectors", {
  testthat::skip_if_offline()
  # Using the tiny example model seeded in tempdir to avoid downloads
  mock_old <- getOption("writeAlizer.mock_dir")
  ex_dir <- writeAlizer::wa_seed_example_models("example", dir = tempdir())
  on.exit(options(writeAlizer.mock_dir = mock_old), add = TRUE)

  coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
  out <- predict_quality("example", coh)

  pred_cols <- grep("^pred_", names(out), value = TRUE)
  expect_true(length(pred_cols) >= 1)
  # ensure no nested data.frames or matrices slipped through
  expect_true(all(vapply(out[pred_cols], is.atomic, logical(1))))
})


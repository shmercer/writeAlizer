# This file includes functions to generate predicted writing quality scores
# and written expression curriculum-based measurement scores (CWS and CIWS)
# from Readerbench, CohMetrix, and/or GAMET files.

#' @title Pre-process data
#' @description Pre-process Coh-Metrix and ReaderBench data files before applying predictive models.
#' Uses the artifact registry to load the correct variable lists and applies
#' centering and scaling per sub-model, preserving the original behavior by model key.
#' @importFrom caret preProcess
#' @importFrom tidyselect all_of
#' @param model Character scalar. Which scoring model to use. Supported values include:
#'   ReaderBench: 'rb_mod1','rb_mod2','rb_mod3narr','rb_mod3exp','rb_mod3per','rb_mod3all',
#'   'rb_mod3narr_v2','rb_mod3exp_v2','rb_mod3per_v2','rb_mod3all_v2';
#'   Coh-Metrix: 'coh_mod1','coh_mod2','coh_mod3narr','coh_mod3exp','coh_mod3per','coh_mod3all';
#'   GAMET: 'gamet_cws1'.
#'   Legacy keys for RB mod3 (non-v2) are mapped to their v2 equivalents internally.
#' @param data A data.frame produced by \code{\link{import_rb}}, \code{\link{import_coh}},
#'   or \code{\link{import_gamet}}, with an \code{ID} column and the expected feature columns.
#' @return A list of pre-processed data frames, one per sub-model. For models with no
#'   varlists (e.g., 'rb_mod1','coh_mod1'), returns six copies of the input data.
#'   For 'gamet_cws1', returns two copies (CWS/CIWS). For 1-part/3-part models, returns
#'   a list of length 1/3 with centered & scaled features plus the \code{ID} column.
#' @export
#' @details
#' **Offline/examples:** Examples use a built-in 'example' model seeded in a temporary
#' directory via \code{writeAlizer::wa_seed_example_models("example")}, so no downloads
#' are attempted and checks stay fast.
#' @examples
#' # Minimal, offline example using the built-in 'example' model (no downloads)
#' rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
#' rb <- import_rb(rb_path)
#'
#' pp <- preprocess("example", rb)
#' length(pp); lapply(pp, nrow)
preprocess <- function(model, data) {
  # Map legacy keys (e.g., rb_mod3narr -> rb_mod3narr_v2) to the canonical key if available
  key <- if (exists(".wa_canonical_model", mode = "function")) .wa_canonical_model(model) else model

  # 'example' is a tiny, offline demo model — no varlists, one split
  if (identical(key, "example")) {
    return(list(data["ID"]))
  }

  # Models with no varlists: preserve existing behavior
  if (model %in% c("rb_mod1", "coh_mod1")) {
    # Return 6 copies (downstream expects 6 submodels)
    return(list(data, data, data, data, data, data))
  }
  if (model == "gamet_cws1") {
    # Return 2 splits (CWS & CIWS)
    return(list(data, data))
  }

  # Load variable lists (RDS) from the registry/cache
  rds_parts <- .wa_parts_for(kind = "rds", model = key)
  if (nrow(rds_parts) == 0L) {
    stop(sprintf("No variable lists registered for model '%s' (canonical: '%s')", model, key), call. = FALSE)
  }

  varlists <- lapply(seq_len(nrow(rds_parts)), function(i) {
    p <- rds_parts[i, ]
    readRDS(.wa_ensure_file(p$file, p$url, sha256 = if ("sha" %in% names(rds_parts)) rds_parts$sha[i] else NULL))
  })

  # Helper to center/scale a slice and keep ID
  prep_slice <- function(vars) {
    data_i <- dplyr::select(data, tidyselect::all_of(vars))
    pp     <- caret::preProcess(data_i, method = c("center", "scale"))
    data_s <- stats::predict(pp, data_i)
    data.frame(ID = data$ID, data_s, check.names = FALSE)
  }

  # 3-part models
  if (model %in% c("rb_mod2", "coh_mod2", "rb_mod3all", "rb_mod3all_v2", "coh_mod3all")) {
    if (length(varlists) < 3L) stop(sprintf("Expected 3 varlists for model '%s'", model), call. = FALSE)
    return(list(
      prep_slice(varlists[[1L]]),
      prep_slice(varlists[[2L]]),
      prep_slice(varlists[[3L]])
    ))
  }

  # 1-part models
  if (model %in% c("rb_mod3narr", "rb_mod3exp", "rb_mod3per",
                   "coh_mod3narr", "coh_mod3exp", "coh_mod3per",
                   "rb_mod3narr_v2", "rb_mod3exp_v2", "rb_mod3per_v2")) {
    return(list(prep_slice(varlists[[1L]])))
  }

  stop(sprintf("Unknown model key '%s'", model), call. = FALSE)
}

#' @title Predict writing quality
#' @description Run the specified model(s) on preprocessed data and return predictions.
#' Apply scoring models to ReaderBench, Coh-Metrix, and/or GAMET files. Holistic
#' writing quality can be generated from ReaderBench (model = 'rb_mod3all') or
#' Coh-Metrix files (model = 'coh_mod3all'). Also, Correct Word Sequences and
#' Correct Minus Incorrect Word Sequences can be generated from a GAMET file
#' (model = 'gamet_cws1').
#' @importFrom utils write.table
#' @importFrom stats predict
#' @importFrom dplyr select
#' @param model A string telling which scoring model to use.
#' Options are:
#' 'rb_mod1', 'rb_mod2', 'rb_mod3narr', 'rb_mod3exp',
#' 'rb_mod3per', or 'rb_mod3all', for ReaderBench files to generate holistic quality,
#' 'coh_mod1', 'coh_mod2', 'coh_mod3narr', 'coh_mod3exp', 'coh_mod3per',
#' or 'coh_mod3all' for Coh-Metrix files to generate holistic quality,
#' and 'gamet_cws1' to generate Correct Word Sequences (CWS)
#' and Correct Minus Incorrect Word Sequences (CIWS) scores from a GAMET file.
#' @param data Data frame returned by \code{\link{import_gamet}},
#'   \code{\link{import_coh}}, or \code{\link{import_rb}}.
#' @return A \code{data.frame} with \code{ID} and one column per sub-model prediction.
#'         If multiple sub-models are used and all predictions are numeric,
#'         an aggregate column named \code{pred_<model>_mean} is added
#'         (except for "gamet_cws1").
#' @seealso \code{\link{import_rb}}, \code{\link{import_coh}}, \code{\link{import_gamet}}
#' @details
#' **Offline/examples:** Examples use a built-in 'example' model seeded in a temporary
#' directory via \code{writeAlizer::wa_seed_example_models("example")}, so no downloads
#' are attempted and checks stay fast. The temporary files created for the example are
#' cleaned up at the end of the \code{\\examples{}}.
#' @export
#' @examples
#' # Fast, offline example: seed a tiny 'example' model and predict (no downloads)
#' coh_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
#' coh <- import_coh(coh_path)
#'
#' mock_old <- getOption("writeAlizer.mock_dir")
#' ex_dir <- writeAlizer::wa_seed_example_models("example", dir = tempdir())
#' on.exit(options(writeAlizer.mock_dir = mock_old), add = TRUE)
#'
#' out <- predict_quality("example", coh)
#' head(out)
#'
#' # IMPORTANT: reset mock_dir before running full demos, so real artifacts load
#' options(writeAlizer.mock_dir = mock_old)
#'
#' \dontshow{
#' # Cleanup of example artifacts created under tempdir()
#' if (is.character(ex_dir) && nzchar(ex_dir) && dir.exists(ex_dir)) {
#'   unlink(ex_dir, recursive = TRUE, force = TRUE)
#' }
#' }
#'
#' # More complete demos (skipped on CRAN to keep checks fast)
#' \donttest{
#'   ### Example 1: ReaderBench output file
#'   file_path1 <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
#'   rb_file <- import_rb(file_path1)
#'   rb_quality <- predict_quality("rb_mod3all", rb_file)
#'   head(rb_quality)
#'
#'   ### Example 2: Coh-Metrix output file
#'   file_path2 <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
#'   coh_file <- import_coh(file_path2)
#'   coh_quality <- predict_quality("coh_mod3all", coh_file)
#'   head(coh_quality)
#'
#'   ### Example 3: GAMET output file (CWS and CIWS)
#'   file_path3 <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
#'   gam_file <- import_gamet(file_path3)
#'   gamet_CWS_CIWS <- predict_quality("gamet_cws1", gam_file)
#'   head(gamet_CWS_CIWS)
#' }
predict_quality <- function(model, data) {
  stopifnot(is.data.frame(data), "ID" %in% names(data))

  requested_model  <- model                       # for output naming
  canonical_model  <- .wa_canonical_model(model)  # for artifact/varlist loading

  # 1) Preprocess for the canonical model key
  data_pp <- preprocess(canonical_model, data)

  # 2) Load trained fits for the canonical model
  fits <- .wa_load_fits_list(canonical_model)

  # 3) Expected object names by canonical key
  fit_names <- switch(
    canonical_model,
    "rb_mod1"        = paste0("rb_mod1",  letters[1:6]),
    "coh_mod1"       = paste0("coh_mod1", letters[1:6]),
    "rb_mod2"        = paste0("rb_mod2",  letters[1:3]),
    "coh_mod2"       = paste0("coh_mod2",  letters[1:3]),
    "rb_mod3all_v2"  = c("rb_mod3exp_v2", "rb_mod3narr_v2", "rb_mod3per_v2"),
    "rb_mod3narr_v2" = "rb_mod3narr_v2",
    "rb_mod3exp_v2"  = "rb_mod3exp_v2",
    "rb_mod3per_v2"  = "rb_mod3per_v2",
    "coh_mod3all"    = c("coh_mod3exp",   "coh_mod3narr",   "coh_mod3per"),
    "coh_mod3narr"   = "coh_mod3narr",
    "coh_mod3exp"    = "coh_mod3exp",
    "coh_mod3per"    = "coh_mod3per",
    "gamet_cws1"     = c("CWS_mod1a", "CIWS_mod1a"),
    "example"        = "example",                       # <-- NEW
    stop(sprintf("Unknown model key '%s' (canonicalized from '%s')",
                 canonical_model, requested_model))
  )

  if (length(fit_names) != length(data_pp)) {
    stop(sprintf("Mismatch: expected %d sub-models, got %d preprocessed splits.",
                 length(fit_names), length(data_pp)))
  }
  missing <- setdiff(fit_names, names(fits))
  if (length(missing)) {
    stop(sprintf("Missing trained objects for model '%s': %s",
                 canonical_model, paste(missing, collapse = ", ")))
  }

  #outward display names have '_v2' stripped for RB mod3
  strip_v2 <- function(x) sub("_v2$", "", x)
  out_names <- if (grepl("^rb_mod3", canonical_model)) strip_v2(fit_names) else fit_names

  # 4) Predict per sub-model
  drop_id <- function(df) if ("ID" %in% names(df)) df[setdiff(names(df), "ID")] else df
  preds <- vector("list", length(fit_names))
  names(preds) <- out_names

  for (i in seq_along(fit_names)) {
    newx <- drop_id(data_pp[[i]])
    preds[[out_names[[i]]]] <- predict(fits[[fit_names[[i]]]], newdata = newx)
  }

  # 5) Assemble output with outward names
  out <- data.frame(ID = data$ID, stringsAsFactors = FALSE)
  for (nm in out_names) out[[paste0("pred_", nm)]] <- preds[[nm]]

  # Mean column: pred_<model>_mean, with any trailing _v2 removed
  pred_cols <- grep("^pred_", names(out), value = TRUE)
  if (requested_model != "gamet_cws1" &&
      length(pred_cols) > 1 &&
      all(vapply(out[pred_cols], is.numeric, logical(1)))) {
    model_for_mean <- sub("_v2$", "", requested_model)
    out[[paste0("pred_", model_for_mean, "_mean")]] <- rowMeans(out[pred_cols], na.rm = TRUE)
  }

  out
}


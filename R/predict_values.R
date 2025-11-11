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
#' Coh-Metrix files (model = 'coh_mod3all'). Also, Total Words Written,
#' Words Spelled Correctly, Correct Word Sequences, and
#' Correct Minus Incorrect Word Sequences can be generated from a GAMET file
#' (model = 'gamet_cws1').
#' @importFrom utils write.table
#' @importFrom stats predict
#' @importFrom dplyr select
#' @importFrom rlang abort
#' @param model A string telling which scoring model to use.
#' Options are:
#' 'rb_mod1', 'rb_mod2', 'rb_mod3narr', 'rb_mod3exp',
#' 'rb_mod3per', or 'rb_mod3all', for ReaderBench files to generate holistic quality,
#' 'coh_mod1', 'coh_mod2', 'coh_mod3narr', 'coh_mod3exp', 'coh_mod3per',
#' or 'coh_mod3all' for Coh-Metrix files to generate holistic quality,
#' and 'gamet_cws1' to generate Total Words Written (TWW), Words Spelled Correctly (WSC),
#' Correct Word Sequences (CWS) and Correct Minus Incorrect Word Sequences (CIWS) scores
#' from a GAMET file.
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
#' @examples
#' # Offline, CRAN-safe example using a tiny seeded model
#' if (requireNamespace("withr", quietly = TRUE)) {
#'   withr::local_options(writeAlizer.offline = TRUE)
#'   tmp <- withr::local_tempdir()
#'   withr::local_options(writeAlizer.mock_dir = tmp)
#'
#'   # Seed the example artifacts into the temp dir and point the loader there
#'   writeAlizer::wa_seed_example_models("example", dir = tmp)
#'
#'   coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
#'   out <- predict_quality("example", coh)
#'   head(out)
#' } else {
#'   # Fallback without 'withr' (still CRAN-safe)
#'   old <- options(writeAlizer.offline = TRUE)
#'   on.exit(options(old), add = TRUE)
#'   ex_dir <- writeAlizer::wa_seed_example_models("example", dir = tempdir())
#'   old2 <- options(writeAlizer.mock_dir = ex_dir)
#'   on.exit(options(old2), add = TRUE)
#'
#'   coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
#'   out <- predict_quality("example", coh)
#'   head(out)
#' }
#'
#' # Longer, networked demos (skipped on CRAN)
#' \donttest{
#' if (!isTRUE(getOption("writeAlizer.offline", FALSE))) {
#'   rb <- import_rb(system.file("extdata", "sample_rb.csv", package = "writeAlizer"))
#'   print(head(predict_quality("rb_mod3all", rb)))
#'
#'   coh <- import_coh(system.file("extdata", "sample_coh.csv", package = "writeAlizer"))
#'   print(head(predict_quality("coh_mod3all", coh)))
#'
#'   gam <- import_gamet(system.file("extdata", "sample_gamet.csv", package = "writeAlizer"))
#'   print(head(predict_quality("gamet_cws1", gam)))
#' }
#' }
#' @export
predict_quality <- function(model, data) {
  # ---- Argument validation with helpful guidance ----
  # Catch common mistake where args are flipped:
  if (is.data.frame(model) && !missing(data)) {
    rlang::abort(
      paste0(
        "It looks like you passed `data` as the first argument.\n",
        "The function signature is predict_quality(model, data).\n\n",
        "Try one of:\n",
        "  predict_quality(\"rb_mod3all\", your_data)\n",
        "  predict_quality(model = \"rb_mod3all\", data = your_data)"
      ),
      .subclass = "writeAlizer_input_error"
    )
  }

  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    rlang::abort(
      "`model` must be a non-empty character scalar (e.g., \"rb_mod3all\").",
      .subclass = "writeAlizer_input_error"
    )
  }

  if (!is.data.frame(data)) {
    rlang::abort(
      "`data` must be a data.frame produced by import_rb(), import_coh(), or import_gamet().",
      .subclass = "writeAlizer_input_error"
    )
  }

  if (!"ID" %in% names(data)) {
    rlang::abort(
      "`data` must include an `ID` column.",
      .subclass = "writeAlizer_input_error"
    )
  }

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
    "example"        = "example",
    {
      valid <- c(
        "rb_mod1","rb_mod2","rb_mod3narr","rb_mod3exp","rb_mod3per","rb_mod3all",
        "coh_mod1","coh_mod2","coh_mod3narr","coh_mod3exp","coh_mod3per","coh_mod3all",
        "gamet_cws1","example"
      )
      rlang::abort(
        sprintf(
          "Unknown model key '%s' (canonicalized from '%s'). Valid options are: %s.\nSee ?predict_quality for details.",
          canonical_model, requested_model, paste(valid, collapse = ", ")
        ),
        .subclass = "writeAlizer_model_unknown"
      )
    }
  )

  if (length(fit_names) != length(data_pp)) {
    rlang::abort(
      sprintf(
        "Internal mismatch: expected %d sub-model(s), but preprocessing produced %d split(s).",
        length(fit_names), length(data_pp)
      ),
      .subclass = "writeAlizer_internal_mismatch"
    )
  }

  missing <- setdiff(fit_names, names(fits))
  if (length(missing)) {
    mock_dir <- getOption("writeAlizer.mock_dir")
    hint <- if (is.character(mock_dir) && nzchar(mock_dir)) {
      sprintf(
        "\nNote: writeAlizer.mock_dir is set to '%s'. If you're running the offline demo, (re)seed with writeAlizer::wa_seed_example_models(\"example\") or clear the option.",
        mock_dir
      )
    } else {
      ""
    }
    rlang::abort(
      paste0(
        sprintf("Missing trained objects for model '%s': %s.",
                canonical_model, paste(missing, collapse = ", ")),
        hint
      ),
      .subclass = "writeAlizer_artifact_missing"
    )
  }

  # outward display names have '_v2' stripped for RB mod3
  strip_v2 <- function(x) sub("_v2$", "", x)
  out_names <- if (grepl("^rb_mod3", canonical_model)) strip_v2(fit_names) else fit_names

  # 4) Predict per sub-model (normalize each prediction to a plain vector)
  drop_id <- function(df) if ("ID" %in% names(df)) df[setdiff(names(df), "ID")] else df
  preds <- vector("list", length(fit_names))
  names(preds) <- out_names

  for (i in seq_along(fit_names)) {
    newx <- drop_id(data_pp[[i]])
    p <- predict(fits[[fit_names[[i]]]], newdata = newx)

    # --- Robust coercion so we always assign a simple numeric/character vector ---
    if (is.data.frame(p)) p <- p[[1]]
    if (is.matrix(p))     p <- p[, 1, drop = TRUE]
    preds[[out_names[[i]]]] <- p
  }

  # 5) Assemble output with outward names
  out <- data.frame(ID = data$ID, stringsAsFactors = FALSE)
  for (nm in out_names) out[[paste0("pred_", nm)]] <- preds[[nm]]

  # 6) Add mean column when appropriate: pred_<model>_mean (skip for GAMET)
  pred_cols <- grep("^pred_", names(out), value = TRUE)
  if (canonical_model != "gamet_cws1" &&
      length(pred_cols) > 1 &&
      all(vapply(out[pred_cols], is.numeric, logical(1)))) {
    model_for_mean <- sub("_v2$", "", requested_model)
    out[[paste0("pred_", model_for_mean, "_mean")]] <- rowMeans(out[pred_cols], na.rm = TRUE)
  }

  # 7) GAMET enhancements: add pred_TWW_gamet and pred_WSC_gamet; order outputs
     if (identical(canonical_model, "gamet_cws1")) {
        wc  <- suppressWarnings(as.numeric(data[["word_count"]]))
        mis <- suppressWarnings(as.numeric(data[["misspelling"]]))

        # Add new derived predictions
        out[["pred_TWW_gamet"]] <- wc
        out[["pred_WSC_gamet"]] <- wc - mis

        # Reorder columns: ID, pred_TWW_gamet, pred_WSC_gamet, pred_CWS_mod1a, pred_CIWS_mod1a
        order_cols <- c("ID", "pred_TWW_gamet", "pred_WSC_gamet",
                        "pred_CWS_mod1a", "pred_CIWS_mod1a")
        rest <- setdiff(names(out), order_cols)
        out <- out[c(order_cols, rest)]
      }

  out
}

# writeAlizer: An R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores
# Copyright (C) 2020 Sterett H. Mercer
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see https://www.gnu.org/licenses/.
#
# This file includes functions to generate predicted writing quality scores
# and written expression curriculum-based measurement scores (CWS and CIWS)
# from Readerbench, CohMetrix, and/or GAMET files.

#' @title Download model objects
#' @description Download model objects or variable lists
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca>
#' @importFrom utils download.file
#' @param file A string that provides the file name, for example "rb_mod1a.rda"
#' @param url The location of the file, for example "https://osf.io/eq9rw/download"
#' @export
#' @examples
#' #load package
#' library(writeAlizer)
#'
#' #download rb_mod1a to extdata
#' download("rb_mod1a.rda", "https://osf.io/eq9rw/download")
download <- function(file, url){
  path <- system.file("extdata", package = "writeAlizer")
  file <- paste(path, file, sep = "/")
  download.file(url= url,
                destfile=file, mode = "wb")
}

#' @title Pre-process data
#' @description Pre-process Coh-Metrix and ReaderBench data files before applying predictive models
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca>
#' @importFrom caret preProcess
#' @importFrom tidyselect all_of
#' @param model A string telling which scoring model to use.
#' Options are:
#' 'rb_mod1', 'rb_mod2', 'rb_mod3narr', 'rb_mod3exp',
#' 'rb_mod3per', 'rb_mod3all', 'rb_mod3narr_v2', 'rb_mod3exp_v2',
#' 'rb_mod3per_v2', or 'rb_mod3all_v2' for ReaderBench files,
#' 'coh_mod1', 'coh_mod2', 'coh_mod3narr', 'coh_mod3exp', 'coh_mod3per'
#'  or 'coh_mod3all' for Coh-Metrix files, or
#' 'gamet_cws1' for GAMET files
#' @param data The name of the R object corresponding to the data file. Use
#' \code{\link{import_gamet}}, \code{\link{import_coh}}, or \code{\link{import_rb}}
#' before this function to generate these data objects.
#' @return A list of pre-processed data frames, one per sub-model
#' @export
preprocess <- function(model, data) {
  # ----------------------------
  # 0) Model family counts (for parity with existing behavior)
  # ----------------------------
  if (model %in% c("rb_mod1", "coh_mod1")) {
    mod_count <- 6
  } else if (model == "gamet_cws1") {
    mod_count <- 1
  } else {
    parts_tmp <- .wa_parts_for(kind = "rds", model = model)
    if (nrow(parts_tmp) == 0L) {
      stop(sprintf("No variable lists registered for model '%s'", model))
    }
    mod_count <- nrow(parts_tmp)
  }

  # -------------------------------------------
  # 1) Load variable lists via the registry
  # -------------------------------------------
  vars_a <- vars_b <- vars_c <- NULL
  if (!(model %in% c("rb_mod1", "coh_mod1", "gamet_cws1"))) {
    parts <- .wa_parts_for(kind = "rds", model = model)
    varlists <- lapply(seq_len(nrow(parts)), function(i) {
      p <- parts[i, ]
      readRDS(.wa_ensure_file(p$file, p$url))  # downloads to inst/extdata if missing
    })
    if (length(varlists) >= 1) vars_a <- varlists[[1]]
    if (length(varlists) >= 2) vars_b <- varlists[[2]]
    if (length(varlists) >= 3) vars_c <- varlists[[3]]
  }

  # ------------------------------------------------
  # 2) Preserve existing preprocessing by model type
  # ------------------------------------------------
  if (model %in% c("rb_mod1", "coh_mod1")) {
    # No preprocessing; return 6 copies to match downstream expectations
    data_pp <- list(data, data, data, data, data, data)

  } else if (model == "gamet_cws1") {
    # Both CWS and CIWS consume the same features; return two splits
    data_pp <- list(data, data)

  } else if (model %in% c("rb_mod2", "coh_mod2", "rb_mod3all",
                          "rb_mod3all_v2", "coh_mod3all")) {
    # Three-part models
    # Part A
    data1 <- data %>% dplyr::select(all_of(vars_a))
    pp1 <- preProcess(data1, method = c("center", "scale"))
    data1r <- predict(pp1, data1)
    data1pp <- data.frame(cbind(ID = data$ID, data1r))

    # Part B
    data2 <- data %>% dplyr::select(all_of(vars_b))
    pp2 <- preProcess(data2, method = c("center", "scale"))
    data2r <- predict(pp2, data2)
    data2pp <- data.frame(cbind(ID = data$ID, data2r))

    # Part C
    data3 <- data %>% dplyr::select(all_of(vars_c))
    pp3 <- preProcess(data3, method = c("center", "scale"))
    data3r <- predict(pp3, data3)
    data3pp <- data.frame(cbind(ID = data$ID, data3r))

    data_pp <- list(data1pp, data2pp, data3pp)

  } else if (model %in% c("rb_mod3narr", "rb_mod3exp", "rb_mod3per",
                          "coh_mod3narr", "coh_mod3exp", "coh_mod3per",
                          "rb_mod3narr_v2", "rb_mod3exp_v2", "rb_mod3per_v2")) {
    # One-part models
    data1 <- data %>% dplyr::select(all_of(vars_a))
    pp1 <- preProcess(data1, method = c("center", "scale"))
    data1r <- predict(pp1, data1)
    data1pp <- data.frame(cbind(ID = data$ID, data1r))

    data_pp <- list(data1pp)

  } else {
    stop(sprintf("Unknown model key '%s'", model))
  }

  return(data_pp)
}

#' @title Apply scoring models for predictions
#' @description Apply scoring models to ReaderBench, CohMetrix, and/or
#' GAMET files. Holistic writing quality can be
#' generated from Readerbench (model = 'rb_mod2') or
#' Coh-Metrix files (model = 'coh_mod2'). Also,
#' Correct Word Sequences and Correct Minus Incorrect
#' Word Sequences can be generated from a GAMET file (model = 'gamet_cws1').
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca>
#' @importFrom utils write.table
#' @importFrom stats predict
#' @importFrom dplyr select
#' @param model A string telling which scoring model to use.
#' Options are:
#' 'rb_mod1', 'rb_mod2', 'rb_mod3narr', 'rb_mod3exp',
#' 'rb_mod3per', 'rb_mod3all', 'rb_mod3narr_v2', 'rb_mod3exp_v2',
#' 'rb_mod3per_v2', or 'rb_mod3all_v2' for ReaderBench files to generate holistic quality,
#' 'coh_mod1', 'coh_mod2' 'coh_mod3narr', 'coh_mod3exp', 'coh_mod3per'
#'  or 'coh_mod3all' for Coh-Metrix files to generate holistic quality,
#' and 'gamet_cws1' to generate Correct Word Sequences (CWS)
#' and Correct Minus Incorrect Word Sequences (CIWS) scores from a GAMET file.
#' @param data The name of the R object corresponding to the data file. The
#' \code{\link{import_gamet}}import_gamet(), \code{\link{import_coh}}import_coh(), or
#' \code{\link{import_rb}}import_rb()
#' functions should be used before this function
#' to generate these data objects.
#' @return Depending on the model parameter option selected, predicted quality (or CWS/CIWS scores)
#' and the ID variable (parsed from the file names used when generating the ReaderBench, Coh-Metrix,
#' and/or GAMET output files) are returned.
#' @export
#' @seealso
#' \code{\link{import_rb}}
#' \code{\link{import_coh}}
#' \code{\link{import_gamet}}
#' @examples
#' \donttest{
#' ###Examples using sample data included in writeAlizer package
#'
#' ##Example 1: ReaderBench output file
#' #load package
#' library(writeAlizer)
#'
#' #get path of sample ReaderBench output file
#' file_path1 <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
#'
#' #see path to sample file
#' file_path1
#'
#' #import file and store as "rb_file"
#' rb_file <- import_rb(file_path1)
#'
#' #Generate holistic quality from "rb_file"
#' #and return scores to an object called "rb_quality":
#' rb_quality <- predict_quality('rb_mod2', rb_file)
#'
#' #display quality scores
#' rb_quality
#'
#' ##Example 2: Coh-Metrix output file
#' #get path of sample Coh-Metrix output file
#' file_path2 <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
#'
#' #see path to sample file
#' file_path2
#'
#' #import file and store as "coh_file"
#' coh_file <- import_coh(file_path2)
#'
#' #Generate holistic quality from a Coh-Metrix file (coh_file),
#' #return scores to an object called "coh_quality",
#' coh_quality <- predict_quality('coh_mod2', coh_file)
#'
#' #display quality scores
#' coh_quality
#'
#' ##Example 3: GAMET output file
#' #get path of sample GAMET output file
#' file_path3 <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
#'
#' #see path to sample GAMET file
#' file_path3
#'
#' #import files, merge, and store as "gam_file"
#' gam_file <- import_gamet(file_path3)
#'
#' #Generate CWS and CIWS scores from a GAMET file
#' #(gam_file) and return scores to an object called "gamet_CWS_CIWS"
#' gamet_CWS_CIWS <- predict_quality('gamet_cws1', gam_file)
#'
#' #display quality scores
#' gamet_CWS_CIWS
#' }
predict_quality <- function(model, data) {
  stopifnot(is.data.frame(data), "ID" %in% names(data))

  # 1) Preprocess (registry-driven varlists)
  data_pp <- preprocess(model, data)  # list length 1, 3, or 6 (or 1 for gamet)

  # 2) Load trained fits as a named list (canonical names from filenames)
  fits <- .wa_load_fits_list(model)

  # 3) Expected order/names per model key
  obj_names <- switch(
    model,
    # 6-part models
    "rb_mod1"  = paste0("rb_mod1",  letters[1:6]),
    "coh_mod1" = paste0("coh_mod1", letters[1:6]),
    # 3-part models
    "rb_mod2"       = paste0("rb_mod2",  letters[1:3]),
    "coh_mod2"      = paste0("coh_mod2",  letters[1:3]),
    "rb_mod3all"    = c("rb_mod3exp", "rb_mod3narr", "rb_mod3per"),
    "rb_mod3all_v2" = c("rb_mod3exp_v2", "rb_mod3narr_v2", "rb_mod3per_v2"),
    "coh_mod3all"   = c("coh_mod3exp",  "coh_mod3narr",  "coh_mod3per"),
    # single-part models
    "rb_mod3narr"   = "rb_mod3narr",
    "rb_mod3exp"    = "rb_mod3exp",
    "rb_mod3per"    = "rb_mod3per",
    "rb_mod3narr_v2"= "rb_mod3narr_v2",
    "rb_mod3exp_v2" = "rb_mod3exp_v2",
    "rb_mod3per_v2" = "rb_mod3per_v2",
    "coh_mod3narr"  = "coh_mod3narr",
    "coh_mod3exp"   = "coh_mod3exp",
    "coh_mod3per"   = "coh_mod3per",
    # GAMET
    "gamet_cws1"    = c("CWS_mod1a", "CIWS_mod1a"),
    stop(sprintf("Unknown model key '%s'", model))
  )

  # 4) Validate we have a 1:1 mapping between fits and preprocessed splits
  if (length(obj_names) != length(data_pp)) {
    stop(sprintf("Mismatch: expected %d sub-models, got %d preprocessed splits.",
                 length(obj_names), length(data_pp)))
  }
  missing <- setdiff(obj_names, names(fits))
  if (length(missing)) {
    stop(sprintf("Missing trained objects for model '%s': %s",
                 model, paste(missing, collapse = ", ")))
  }

  # 5) Predict per sub-model
  drop_id <- function(df) if ("ID" %in% names(df)) df[setdiff(names(df), "ID")] else df

  preds <- vector("list", length(obj_names))
  names(preds) <- obj_names
  for (i in seq_along(obj_names)) {
    nm <- obj_names[[i]]
    newx <- drop_id(data_pp[[i]])
    preds[[i]] <- predict(fits[[nm]], newdata = newx)
  }

  # 6) Assemble output
  out <- data.frame(ID = data$ID, stringsAsFactors = FALSE)
  for (nm in obj_names) {
    out[[paste0("pred_", nm)]] <- preds[[nm]]
  }

  # 7) If all prediction columns are numeric and we have >1, add a row-mean
  #    BUT: not for GAMET (gamet_cws1)
  pred_cols <- grep("^pred_", names(out), value = TRUE)
  if (
    model != "gamet_cws1" &&
    length(pred_cols) > 1 &&
    all(vapply(out[pred_cols], is.numeric, logical(1)))
  ) {
    out$score_mean <- rowMeans(out[pred_cols], na.rm = TRUE)
  }

  out
}

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
# from Readerbench, CohMetrix, and/or GAMET files. There is also a function
# to generate writing quality composites that include
# GAMET spelling and grammar scores

#declare global vars for the model objects used in predict_quality
if(getRversion() >= "2.10")  utils::globalVariables(c("rb_mod1a", "rb_mod1b", "rb_mod1c","rb_mod1d", "rb_mod1e", "rb_mod1f",
                                                      "coh_mod1a","coh_mod1b","coh_mod1c","coh_mod1d","coh_mod1e","coh_mod1f",
                                                      "CWS_mod1a", "CWS_mod1b", "CIWS_mod1a", "CIWS_mod1b"))

#' @title Apply scoring models for predictions
#' @description Apply scoring models to ReaderBench, CohMetrix, and/or
#' GAMET files. Holistic writing quality can be
#' generated from Readerbench (model = 'rb_mod1') or
#' Coh-Metrix files (model = 'coh_mod1'). Also,
#' Correct Word Sequences and Correct Minus Incorrect
#' Word Sequences can be generated from a combined
#' Readerbench and GAMET file.
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca>
#' @importFrom utils write.table download.file
#' @import caretEnsemble
#' @param model A string telling which scoring model to use.
#' Options are:
#' 'rb_mod1' for ReaderBench files to generate holistic quality,
#' 'coh_mod1' for Coh-Metrix files to generate holistic quality,
#' and 'rb_gamet_cws1' for a merged ReaderBench and GAMET file
#' to generate Correct Word Sequences (CWS) and Correct Minus Incorrect
#' Word Sequences (CIWS) scores
#' @param data The name of the R object corresponding to the data file. The
#' \code{\link{import_gamet}}import_gamet(), \code{\link{import_coh}}import_coh(),
#' \code{\link{import_rb}}import_rb(), and \code{\link{import_merge_gamet_rb}}import_merge_gamet_rb()
#' functions should be used before this function
#' to generate these data objects.
#' @param store When store = TRUE, this function will generate scores, merge
#' the scores into the data file, and export the file as .csv in the
#' working directory. When store = FALSE (the default) the predicted scores
#' are returned to a user-specified object or the R console.
#' @param name When store = TRUE, the name parameter gives the filename for
#' the .csv file (for example, "filename.csv") that is
#' generated to the working directory.
#' @return Depending on the model parameter option selected, predicted quality (or CWS/CIWS scores)
#' and the ID variable (parsed from the file names used when generating the ReaderBench, Coh-Metrix,
#' and/or GAMET output files) are returned.
#' @export
#' @seealso
#' \code{\link{import_rb}}
#' \code{\link{import_coh}}
#' \code{\link{import_gamet}}
#' \code{\link{import_merge_gamet_rb}}
#' @examples
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
#' rb_quality <- predict_quality('rb_mod1', rb_file, store = FALSE)
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
#' #return scores to an object called "coh_quality", merge
#' #predicted scores with the data file, and store data as "output.csv" in the
#' #working directory
#' coh_quality <- predict_quality('coh_mod1', coh_file, store = TRUE, name = "output.csv")
#'
#' #display quality scores
#' coh_quality
#'
#' ##Example 3: Merged ReaderBench and GAMET files
#' #see path to sample ReaderBench file
#' file_path1
#'
#' #get path of sample GAMET output file
#' file_path3 <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
#'
#' #see path to sample GAMET file
#' file_path3
#'
#' #import files, merge, and store as "rb_gam_file"
#' rb_gam_file <- import_merge_gamet_rb(file_path1, file_path3)
#'
#' #Generate CWS and CIWS scores from a merged ReaderBench and GAMET file
#' #(rb_gam_file) and return scores to an object called "rb_gamet_CWS_CIWS"
#' rb_gamet_CWS_CIWS <- predict_quality('rb_gamet_cws1', rb_gam_file, store = FALSE)
#'
#' #display quality scores
#' rb_gamet_CWS_CIWS
predict_quality <- function(model, data, store = FALSE, name = "filename.csv") {
  #download function
  download <- function(mod, url){
    path <- system.file("extdata", package = "writeAlizer")
    file <- paste(path, mod, sep = "/")
    download.file(url= url,
                  destfile=file, mode = "wb")
  }

  if (model=='rb_mod1' & store == FALSE){
    #check if each model object exists, if so, load it; if not, download and load it
    path_1a <- system.file("extdata", "rb_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_1a) == TRUE){
      load(system.file("extdata", "rb_mod1a.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1a.rda",
               "https://www.dropbox.com/s/qse8olc328ax54w/rb_mod1a.rda?dl=1")
      load(system.file("extdata", "rb_mod1a.rda", package = "writeAlizer"))
    }
    path_1b <- system.file("extdata", "rb_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_1b) == TRUE){
      load(system.file("extdata", "rb_mod1b.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1b.rda",
               "https://www.dropbox.com/s/8w9yvnbx30zqzh2/rb_mod1b.rda?dl=1")
      load(system.file("extdata", "rb_mod1b.rda", package = "writeAlizer"))
    }
    path_1c <- system.file("extdata", "rb_mod1c.rda", package = "writeAlizer")
    if (file.exists(path_1c) == TRUE){
      load(system.file("extdata", "rb_mod1c.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1c.rda",
               "https://www.dropbox.com/s/35img463zipn3ch/rb_mod1c.rda?dl=1")
      load(system.file("extdata", "rb_mod1c.rda", package = "writeAlizer"))
    }
    path_1d <- system.file("extdata", "rb_mod1d.rda", package = "writeAlizer")
    if (file.exists(path_1d) == TRUE){
      load(system.file("extdata", "rb_mod1d.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1d.rda",
               "https://www.dropbox.com/s/70no3hgu1rbad0t/rb_mod1d.rda?dl=1")
      load(system.file("extdata", "rb_mod1d.rda", package = "writeAlizer"))
    }
    path_1e <- system.file("extdata", "rb_mod1e.rda", package = "writeAlizer")
    if (file.exists(path_1e) == TRUE){
      load(system.file("extdata", "rb_mod1e.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1e.rda",
               "https://www.dropbox.com/s/1iy2t8ldok7x7jt/rb_mod1e.rda?dl=1")
      load(system.file("extdata", "rb_mod1e.rda", package = "writeAlizer"))
    }
    path_1f <- system.file("extdata", "rb_mod1f.rda", package = "writeAlizer")
    if (file.exists(path_1f) == TRUE){
      load(system.file("extdata", "rb_mod1f.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1f.rda",
               "https://www.dropbox.com/s/w50pv4ozd1feo8j/rb_mod1f.rda?dl=1")
      load(system.file("extdata", "rb_mod1f.rda", package = "writeAlizer"))
    }

    pred.1 <- predict(rb_mod1a,data)
    pred.2 <- predict(rb_mod1b,data)
    pred.3 <- predict(rb_mod1c,data)
    pred.4 <- predict(rb_mod1d,data)
    pred.5 <- predict(rb_mod1e,data)
    pred.6 <- predict(rb_mod1f,data)

    predicted_quality <- (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
    id_quality <- data.frame(cbind(ID=data$ID, predicted_quality))
    return(id_quality)
  } else if (model=='rb_mod1' & store == TRUE){
    #check if each model object exists, if so, load it; if not, download and load it
    path_1a <- system.file("extdata", "rb_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_1a) == TRUE){
      load(system.file("extdata", "rb_mod1a.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1a.rda",
               "https://www.dropbox.com/s/qse8olc328ax54w/rb_mod1a.rda?dl=1")
      load(system.file("extdata", "rb_mod1a.rda", package = "writeAlizer"))
    }
    path_1b <- system.file("extdata", "rb_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_1b) == TRUE){
      load(system.file("extdata", "rb_mod1b.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1b.rda",
               "https://www.dropbox.com/s/8w9yvnbx30zqzh2/rb_mod1b.rda?dl=1")
      load(system.file("extdata", "rb_mod1b.rda", package = "writeAlizer"))
    }
    path_1c <- system.file("extdata", "rb_mod1c.rda", package = "writeAlizer")
    if (file.exists(path_1c) == TRUE){
      load(system.file("extdata", "rb_mod1c.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1c.rda",
               "https://www.dropbox.com/s/35img463zipn3ch/rb_mod1c.rda?dl=1")
      load(system.file("extdata", "rb_mod1c.rda", package = "writeAlizer"))
    }
    path_1d <- system.file("extdata", "rb_mod1d.rda", package = "writeAlizer")
    if (file.exists(path_1d) == TRUE){
      load(system.file("extdata", "rb_mod1d.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1d.rda",
               "https://www.dropbox.com/s/70no3hgu1rbad0t/rb_mod1d.rda?dl=1")
      load(system.file("extdata", "rb_mod1d.rda", package = "writeAlizer"))
    }
    path_1e <- system.file("extdata", "rb_mod1e.rda", package = "writeAlizer")
    if (file.exists(path_1e) == TRUE){
      load(system.file("extdata", "rb_mod1e.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1e.rda",
               "https://www.dropbox.com/s/1iy2t8ldok7x7jt/rb_mod1e.rda?dl=1")
      load(system.file("extdata", "rb_mod1e.rda", package = "writeAlizer"))
    }
    path_1f <- system.file("extdata", "rb_mod1f.rda", package = "writeAlizer")
    if (file.exists(path_1f) == TRUE){
      load(system.file("extdata", "rb_mod1f.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1f.rda",
               "https://www.dropbox.com/s/w50pv4ozd1feo8j/rb_mod1f.rda?dl=1")
      load(system.file("extdata", "rb_mod1f.rda", package = "writeAlizer"))
    }

    pred.1 <- predict(rb_mod1a,data)
    pred.2 <- predict(rb_mod1b,data)
    pred.3 <- predict(rb_mod1c,data)
    pred.4 <- predict(rb_mod1d,data)
    pred.5 <- predict(rb_mod1e,data)
    pred.6 <- predict(rb_mod1f,data)

    predicted_quality <- (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
    data.2 <- cbind(predicted_quality,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)

    id_quality <- data.frame(cbind(ID=data$ID, predicted_quality))
    return(id_quality)
  } else if (model == 'coh_mod1' & store == FALSE){
    #check if each model object exists, if so, load it; if not, download and load it
    path_1a <- system.file("extdata", "coh_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_1a) == TRUE){
      load(system.file("extdata", "coh_mod1a.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1a.rda",
               "https://www.dropbox.com/s/4666echdjxj3bie/coh_mod1a.rda?dl=1")
      load(system.file("extdata", "coh_mod1a.rda", package = "writeAlizer"))
    }
    path_1b <- system.file("extdata", "coh_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_1b) == TRUE){
      load(system.file("extdata", "coh_mod1b.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1b.rda",
               "https://www.dropbox.com/s/3qdy9mgqk0tg59n/coh_mod1b.rda?dl=1")
      load(system.file("extdata", "coh_mod1b.rda", package = "writeAlizer"))
    }
    path_1c <- system.file("extdata", "coh_mod1c.rda", package = "writeAlizer")
    if (file.exists(path_1c) == TRUE){
      load(system.file("extdata", "coh_mod1c.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1c.rda",
               "https://www.dropbox.com/s/7yh9yuldzo7fth4/coh_mod1c.rda?dl=1")
      load(system.file("extdata", "coh_mod1c.rda", package = "writeAlizer"))
    }
    path_1d <- system.file("extdata", "coh_mod1d.rda", package = "writeAlizer")
    if (file.exists(path_1d) == TRUE){
      load(system.file("extdata", "coh_mod1d.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1d.rda",
               "https://www.dropbox.com/s/owe4u5yu1tbaumk/coh_mod1d.rda?dl=1")
      load(system.file("extdata", "coh_mod1d.rda", package = "writeAlizer"))
    }
    path_1e <- system.file("extdata", "coh_mod1e.rda", package = "writeAlizer")
    if (file.exists(path_1e) == TRUE){
      load(system.file("extdata", "coh_mod1e.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1e.rda",
               "https://www.dropbox.com/s/ifmrz1j6i9phjry/coh_mod1e.rda?dl=1")
      load(system.file("extdata", "coh_mod1e.rda", package = "writeAlizer"))
    }
    path_1f <- system.file("extdata", "coh_mod1f.rda", package = "writeAlizer")
    if (file.exists(path_1f) == TRUE){
      load(system.file("extdata", "coh_mod1f.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1f.rda",
               "https://www.dropbox.com/s/qftbg2mbig0urvr/coh_mod1f.rda?dl=1")
      load(system.file("extdata", "coh_mod1f.rda", package = "writeAlizer"))
    }

    pred.1 <- predict(coh_mod1a,data)
    pred.2 <- predict(coh_mod1b,data)
    pred.3 <- predict(coh_mod1c,data)
    pred.4 <- predict(coh_mod1d,data)
    pred.5 <- predict(coh_mod1e,data)
    pred.6 <- predict(coh_mod1f,data)

    predicted_quality <- (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
    id_quality <- data.frame(cbind(ID=data$ID, predicted_quality))
    return(id_quality)
  } else if (model=='coh_mod1' & store == TRUE){
    #check if each model object exists, if so, load it; if not, download and load it
    path_1a <- system.file("extdata", "coh_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_1a) == TRUE){
      load(system.file("extdata", "coh_mod1a.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1a.rda",
               "https://www.dropbox.com/s/4666echdjxj3bie/coh_mod1a.rda?dl=1")
      load(system.file("extdata", "coh_mod1a.rda", package = "writeAlizer"))
    }
    path_1b <- system.file("extdata", "coh_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_1b) == TRUE){
      load(system.file("extdata", "coh_mod1b.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1b.rda",
               "https://www.dropbox.com/s/3qdy9mgqk0tg59n/coh_mod1b.rda?dl=1")
      load(system.file("extdata", "coh_mod1b.rda", package = "writeAlizer"))
    }
    path_1c <- system.file("extdata", "coh_mod1c.rda", package = "writeAlizer")
    if (file.exists(path_1c) == TRUE){
      load(system.file("extdata", "coh_mod1c.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1c.rda",
               "https://www.dropbox.com/s/7yh9yuldzo7fth4/coh_mod1c.rda?dl=1")
      load(system.file("extdata", "coh_mod1c.rda", package = "writeAlizer"))
    }
    path_1d <- system.file("extdata", "coh_mod1d.rda", package = "writeAlizer")
    if (file.exists(path_1d) == TRUE){
      load(system.file("extdata", "coh_mod1d.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1d.rda",
               "https://www.dropbox.com/s/owe4u5yu1tbaumk/coh_mod1d.rda?dl=1")
      load(system.file("extdata", "coh_mod1d.rda", package = "writeAlizer"))
    }
    path_1e <- system.file("extdata", "coh_mod1e.rda", package = "writeAlizer")
    if (file.exists(path_1e) == TRUE){
      load(system.file("extdata", "coh_mod1e.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1e.rda",
               "https://www.dropbox.com/s/ifmrz1j6i9phjry/coh_mod1e.rda?dl=1")
      load(system.file("extdata", "coh_mod1e.rda", package = "writeAlizer"))
    }
    path_1f <- system.file("extdata", "coh_mod1f.rda", package = "writeAlizer")
    if (file.exists(path_1f) == TRUE){
      load(system.file("extdata", "coh_mod1f.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1f.rda",
               "https://www.dropbox.com/s/qftbg2mbig0urvr/coh_mod1f.rda?dl=1")
      load(system.file("extdata", "coh_mod1f.rda", package = "writeAlizer"))
    }
    pred.1 <- predict(coh_mod1a,data)
    pred.2 <- predict(coh_mod1b,data)
    pred.3 <- predict(coh_mod1c,data)
    pred.4 <- predict(coh_mod1d,data)
    pred.5 <- predict(coh_mod1e,data)
    pred.6 <- predict(coh_mod1f,data)

    predicted_quality <- (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
    data.2 <- cbind(predicted_quality,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)

    id_quality <- data.frame(cbind(ID=data$ID, predicted_quality))
    return(id_quality)
  } else if (model == 'rb_gamet_cws1' & store == FALSE) {
    #check if each model object exists, if so, load it; if not, download and load it
    path_cws1a <- system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_cws1a) == TRUE){
      load(system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer"))
    } else {
      download("CWS_mod1a.rda",
               "https://www.dropbox.com/s/gmqrc9keio5wnku/CWS_mod1a.rda?dl=1")
      load(system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer"))
    }
    path_cws1b <- system.file("extdata", "CWS_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_cws1b) == TRUE){
      load(system.file("extdata", "CWS_mod1b.rda", package = "writeAlizer"))
    } else {
      download("CWS_mod1b.rda",
               "https://www.dropbox.com/s/dzfeaa6g7lh8owm/CWS_mod1b.rda?dl=1")
      load(system.file("extdata", "CWS_mod1b.rda", package = "writeAlizer"))
    }
    path_ciws1a <- system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_ciws1a) == TRUE){
      load(system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer"))
    } else {
      download("CIWS_mod1a.rda",
               "https://www.dropbox.com/s/b9k8jxh6pyqfs4o/CIWS_mod1a.rda?dl=1")
      load(system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer"))
    }
    path_ciws1b <- system.file("extdata", "CIWS_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_ciws1b) == TRUE){
      load(system.file("extdata", "CIWS_mod1b.rda", package = "writeAlizer"))
    } else {
      download("CIWS_mod1b.rda",
               "https://www.dropbox.com/s/kwb19wgpjyfw89w/CIWS_mod1b.rda?dl=1")
      load(system.file("extdata", "CIWS_mod1b.rda", package = "writeAlizer"))
    }

    cws.pred.1 <- predict(CWS_mod1a, data)
    cws.pred.2 <- predict(CWS_mod1b, data)

    ciws.pred.1 <- predict(CIWS_mod1a, data)
    ciws.pred.2 <- predict(CIWS_mod1b, data)

    predicted_tww <- data$word_count
    predicted_wsc <- data$word_count - data$misspelling

    predicted_cws <- (cws.pred.1 + cws.pred.2)/2
    predicted_ciws <- (ciws.pred.1 + ciws.pred.2)/2

    return(data.frame(cbind(ID=data$ID,predicted_tww, predicted_wsc, predicted_cws,predicted_ciws)))
  } else if (model == 'rb_gamet_cws1' & store == TRUE) {
    #check if each model object exists, if so, load it; if not, download and load it
    path_cws1a <- system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_cws1a) == TRUE){
      load(system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer"))
    } else {
      download("CWS_mod1a.rda",
               "https://www.dropbox.com/s/gmqrc9keio5wnku/CWS_mod1a.rda?dl=1")
      load(system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer"))
    }
    path_cws1b <- system.file("extdata", "CWS_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_cws1b) == TRUE){
      load(system.file("extdata", "CWS_mod1b.rda", package = "writeAlizer"))
    } else {
      download("CWS_mod1b.rda",
               "https://www.dropbox.com/s/dzfeaa6g7lh8owm/CWS_mod1b.rda?dl=1")
      load(system.file("extdata", "CWS_mod1b.rda", package = "writeAlizer"))
    }
    path_ciws1a <- system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_ciws1a) == TRUE){
      load(system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer"))
    } else {
      download("CIWS_mod1a.rda",
               "https://www.dropbox.com/s/b9k8jxh6pyqfs4o/CIWS_mod1a.rda?dl=1")
      load(system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer"))
    }
    path_ciws1b <- system.file("extdata", "CIWS_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_ciws1b) == TRUE){
      load(system.file("extdata", "CIWS_mod1b.rda", package = "writeAlizer"))
    } else {
      download("CIWS_mod1b.rda",
               "https://www.dropbox.com/s/kwb19wgpjyfw89w/CIWS_mod1b.rda?dl=1")
      load(system.file("extdata", "CIWS_mod1b.rda", package = "writeAlizer"))
    }

    cws.pred.1 <- predict(CWS_mod1a, data)
    cws.pred.2 <- predict(CWS_mod1b, data)

    ciws.pred.1 <- predict(CIWS_mod1a, data)
    ciws.pred.2 <- predict(CIWS_mod1b, data)

    predicted_tww <- data$word_count
    predicted_wsc <- data$word_count - data$misspelling

    predicted_cws <- (cws.pred.1 + cws.pred.2)/2
    predicted_ciws <- (ciws.pred.1 + ciws.pred.2)/2

    data.2 <- cbind(predicted_tww, predicted_wsc,predicted_cws,predicted_ciws,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)

    return(data.frame(cbind(ID=data$ID,predicted_tww, predicted_wsc, predicted_cws,predicted_ciws)))
  } else if (model == 'rb_gamet_cws2' & store == FALSE) {
    #check if each model object exists, if so, load it; if not, download and load it
    path_cws2a <- system.file("extdata", "CWS_mod2a.rda", package = "writeAlizer")
    if (file.exists(path_cws2a) == TRUE){
      load(system.file("extdata", "CWS_mod2a.rda", package = "writeAlizer"))
    } else {
      download("CWS_mod2a.rda",
               "https://www.dropbox.com/s/agzvnd7czl0h9vl/CWS_mod2a.rda?dl=1")
      load(system.file("extdata", "CWS_mod2a.rda", package = "writeAlizer"))
    }
    path_ciws2a <- system.file("extdata", "CIWS_mod2a.rda", package = "writeAlizer")
    if (file.exists(path_ciws2a) == TRUE){
      load(system.file("extdata", "CIWS_mod2a.rda", package = "writeAlizer"))
    } else {
      download("CIWS_mod2a.rda",
               "https://www.dropbox.com/s/jaemizeh5sd945v/CIWS_mod2a.rda?dl=1")
      load(system.file("extdata", "CIWS_mod2a.rda", package = "writeAlizer"))
    }

    predicted_tww <- data$word_count
    predicted_wsc <- data$word_count - data$misspelling

    predicted_cws <- predict(CWS_mod2a, data)
    predicted_ciws <- predict(CIWS_mod2a, data)

    return(data.frame(cbind(ID=data$ID,predicted_tww, predicted_wsc, predicted_cws,predicted_ciws)))
  } else if (model == 'rb_gamet_cws2' & store == TRUE) {
    #check if each model object exists, if so, load it; if not, download and load it
    path_cws2a <- system.file("extdata", "CWS_mod2a.rda", package = "writeAlizer")
    if (file.exists(path_cws2a) == TRUE){
      load(system.file("extdata", "CWS_mod2a.rda", package = "writeAlizer"))
    } else {
      download("CWS_mod2a.rda",
               "https://www.dropbox.com/s/agzvnd7czl0h9vl/CWS_mod2a.rda?dl=1")
      load(system.file("extdata", "CWS_mod2a.rda", package = "writeAlizer"))
    }
    path_ciws2a <- system.file("extdata", "CIWS_mod2a.rda", package = "writeAlizer")
    if (file.exists(path_ciws2a) == TRUE){
      load(system.file("extdata", "CIWS_mod2a.rda", package = "writeAlizer"))
    } else {
      download("CIWS_mod2a.rda",
               "https://www.dropbox.com/s/jaemizeh5sd945v/CIWS_mod2a.rda?dl=1")
      load(system.file("extdata", "CIWS_mod2a.rda", package = "writeAlizer"))
    }

    predicted_tww <- data$word_count
    predicted_wsc <- data$word_count - data$misspelling

    predicted_cws <- predict(CWS_mod2a, data)
    predicted_ciws <- predict(CIWS_mod2a, data)

    data.2 <- cbind(predicted_tww, predicted_wsc,predicted_cws,predicted_ciws,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)

    return(data.frame(cbind(ID=data$ID,predicted_tww, predicted_wsc, predicted_cws,predicted_ciws)))
  }
}

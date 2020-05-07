# writeAlizer: An R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores
# Copyright (C) 2020 Sterett H. Mercer

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see https://www.gnu.org/licenses/.

# This file includes functions to generate predicted writing quality values
#from Readerbench and CohMetrix files. There is also a function to generate
#writing quality composites that include GAMET spelling and grammar scores

#' @title Apply scoring models for predictions
#' @description Apply scoring models to ReaderBench, CohMetrix, and/or
#' GAMET files. Holistic writing quality can be
#' generated from Readerbench (model = 'rb_mod1') or
#' Coh-Metrix files (model = 'coh_mod1'). Also,
#' Correct Word Sequences and Correct Minus Incorrect
#' Word Sequences can be generated from a combined
#' Readerbench and GAMET file.
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca>
#' @importFrom utils write.table
#' @import caretEnsemble
#' @param model A string telling which scoring model to use.
#' Options are:
#' 'rb_mod1' for ReaderBench files to generate holistic quality,
#' 'coh_mod1' for Coh-Metrix files to generate holistic quality,
#' and 'rb_gamet_cws1' for a merged ReaderBench and GAMET file
#' to generate Correct Word Sequences (CWS) and Correct Minus Incorrect
#' Word Sequences (CIWS) scores
#' @param data The name of the R object corresponding to the data file. The
#' import_gamet(), import_coh(), import_rb(), and import_merge_gamet_rb()
#' functions should be used before this function
#' to generate these data objects.
#' @param store When store = TRUE, this function will generate scores, merge
#' the scores into the data file, and export the file as .csv in the
#' working directory. When store = FALSE (the default) the predicted scores
#' are returned to a user-specified object or the R console.
#' @param name When store = TRUE, the name parameter gives the filename for
#' the .csv file (for example, "filename.csv") that is
#' generated to the working directory.
#' @export
#' @examples
#' #To generate holistic quality from a ReaderBench file ('rb_data')
#' #and return scores to an object called 'rb_quality':
#' \dontrun{
#' rb_quality <- predict_quality('rb_mod1', rb_data, store = FALSE)
#' }
#' #To generate holistic quality from a Coh-Metrix file ('coh_data'), merge
#' #predicted scores with the data file, and store as "output.csv" in the
#' #working directory
#' \dontrun{
#' predict_quality('coh_mod1', coh_data, store = TRUE, name = "output.csv")
#' }
predict_quality <- function(model, data, store = FALSE, name = "filename.csv") {

  if (model=='rb_mod1' & store == FALSE){
    rb_mod1a <- readRDS('rb_mod1a.rds')
    rb_mod1b <- readRDS('rb_mod1b.rds')
    rb_mod1c <- readRDS('rb_mod1c.rds')
    rb_mod1d <- readRDS('rb_mod1d.rds')
    rb_mod1e <- readRDS('rb_mod1e.rds')
    rb_mod1f <- readRDS('rb_mod1f.rds')


    pred.1 <- predict(rb_mod1a,data)
    pred.2 <- predict(rb_mod1b,data)
    pred.3 <- predict(rb_mod1c,data)
    pred.4 <- predict(rb_mod1d,data)
    pred.5 <- predict(rb_mod1e,data)
    pred.6 <- predict(rb_mod1f,data)

    (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
  } else if (model=='rb_mod1' & store == TRUE){
    rb_mod1a <- readRDS('rb_mod1a.rds')
    rb_mod1b <- readRDS('rb_mod1b.rds')
    rb_mod1c <- readRDS('rb_mod1c.rds')
    rb_mod1d <- readRDS('rb_mod1d.rds')
    rb_mod1e <- readRDS('rb_mod1e.rds')
    rb_mod1f <- readRDS('rb_mod1f.rds')


    pred.1 <- predict(rb_mod1a,data)
    pred.2 <- predict(rb_mod1b,data)
    pred.3 <- predict(rb_mod1c,data)
    pred.4 <- predict(rb_mod1d,data)
    pred.5 <- predict(rb_mod1e,data)
    pred.6 <- predict(rb_mod1f,data)

    predicted_quality <- (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
    data.2 <- cbind(predicted_quality,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)
  } else if (model == 'coh_mod1' & store == FALSE){
    coh_mod1a <- readRDS('coh_1a.rds')
    coh_mod1b <- readRDS('coh_1b.rds')
    coh_mod1c <- readRDS('coh_1c.rds')
    coh_mod1d <- readRDS('coh_1d.rds')
    coh_mod1e <- readRDS('coh_1e.rds')
    coh_mod1f <- readRDS('coh_1f.rds')

    pred.1 <- predict(coh_mod1a,data)
    pred.2 <- predict(coh_mod1b,data)
    pred.3 <- predict(coh_mod1c,data)
    pred.4 <- predict(coh_mod1d,data)
    pred.5 <- predict(coh_mod1e,data)
    pred.6 <- predict(coh_mod1f,data)

    (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
  } else if (model=='coh_mod1' & store == TRUE){
    coh_mod1a <- readRDS('coh_1a.rds')
    coh_mod1b <- readRDS('coh_1b.rds')
    coh_mod1c <- readRDS('coh_1c.rds')
    coh_mod1d <- readRDS('coh_1d.rds')
    coh_mod1e <- readRDS('coh_1e.rds')
    coh_mod1f <- readRDS('coh_1f.rds')

    pred.1 <- predict(coh_mod1a,data)
    pred.2 <- predict(coh_mod1b,data)
    pred.3 <- predict(coh_mod1c,data)
    pred.4 <- predict(coh_mod1d,data)
    pred.5 <- predict(coh_mod1e,data)
    pred.6 <- predict(coh_mod1f,data)

    predicted_quality <- (pred.1 + pred.2 + pred.3 + pred.4 + pred.5 + pred.6)/6
    data.2 <- cbind(predicted_quality,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)
  } else if (model == 'rb_gamet_cws1' & store == FALSE) {
    CWS_mod1a <- readRDS('CWS_mod1a.rds')
    CWS_mod1b <- readRDS('CWS_mod1b.rds')
    CIWS_mod1a <- readRDS('CIWS_mod1a.rds')
    CIWS_mod1b <- readRDS('CIWS_mod1b.rds')

    cws.pred.1 <- predict(CWS_mod1a, data)
    cws.pred.2 <- predict(CWS_mod1b, data)

    ciws.pred.1 <- predict(CIWS_mod1a, data)
    ciws.pred.2 <- predict(CIWS_mod1b, data)

    predicted_cws <- (cws.pred.1 + cws.pred.2)/2
    predicted_ciws <- (ciws.pred.1 + ciws.pred.2)/2

    cbind(predicted_cws,predicted_ciws)
  } else if (model == 'rb_gamet_cws1' & store == TRUE) {
    CWS_mod1a <- readRDS('CWS_mod1a.rds')
    CWS_mod1b <- readRDS('CWS_mod1b.rds')
    CIWS_mod1a <- readRDS('CIWS_mod1a.rds')
    CIWS_mod1b <- readRDS('CIWS_mod1b.rds')

    cws.pred.1 <- predict(CWS_mod1a, data)
    cws.pred.2 <- predict(CWS_mod1b, data)

    ciws.pred.1 <- predict(CIWS_mod1a, data)
    ciws.pred.2 <- predict(CIWS_mod1b, data)

    predicted_cws <- (cws.pred.1 + cws.pred.2)/2
    predicted_ciws <- (ciws.pred.1 + ciws.pred.2)/2

    data.2 <- cbind(predicted_cws,predicted_ciws,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)
  }
}

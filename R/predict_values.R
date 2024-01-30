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

#declare global vars for the model objects used in predict_quality
if(getRversion() >= "2.10")  utils::globalVariables(c("rb_mod1a", "rb_mod1b", "rb_mod1c","rb_mod1d", "rb_mod1e", "rb_mod1f",
                                                      "coh_mod1a","coh_mod1b","coh_mod1c","coh_mod1d","coh_mod1e","coh_mod1f",
                                                      "rb_mod2a", "rb_mod2b", "rb_mod2c",
                                                      "coh_mod2a","coh_mod2b","coh_mod2c",
                                                      "rb_mod3narr", "rb_mod3exp", "rb_mod3per", "rb_mod3all",
                                                      "CWS_mod1a", "CIWS_mod1a"))


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
#' 'rb_mod3per', or 'rb_mod3all' for ReaderBench files,
#' 'coh_mod1' or 'coh_mod2' for Coh-Metrix files, or
#' 'gamet_cws1' for GAMET files
#' @param data The name of the R object corresponding to the data file. The
#' \code{\link{import_gamet}}import_gamet(), \code{\link{import_coh}}import_coh(), or
#' \code{\link{import_rb}}import_rb()
#' functions should be used before this function
#' to generate these data objects.
#' @return returns a list of pre-processed data files, one per sub-model
#' @export
#' @seealso
#' \code{\link{import_rb}}
#' \code{\link{import_coh}}
#' \code{\link{import_gamet}}
#' \code{\link{predict_quality}}
#' @examples
#' ###Example using sample data included in writeAlizer package
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
#' #Pre-process RB data for model 2
#' preprocess('rb_mod2', rb_file)
preprocess <- function(model, data) {
  ##### rb_mod3narr
  #check if the variable list exists, if so, load it; if not, download and load it
  if (model=="rb_mod3narr") {
    path_3narr_vars <- system.file("extdata", "rb_mod3narr_vars.rds", package = "writeAlizer")
    if (file.exists(path_3narr_vars) == TRUE){
      vars_a<-readRDS(system.file("extdata", "rb_mod3narr_vars.rds", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'rb_mod3narr' has been used for predictions, two files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'rb_mod3narr' for predictions.")
      download("rb_mod3narr_vars.rds",
               "https://osf.io/bmqg9/download")
      vars_a<-readRDS(system.file("extdata", "rb_mod3narr_vars.rds", package = "writeAlizer"))
    }
  }
  ##### rb_mod3exp
  #check if the variable list exists, if so, load it; if not, download and load it
  if (model=="rb_mod3exp") {
    path_3exp_vars <- system.file("extdata", "rb_mod3exp_vars.rds", package = "writeAlizer")
    if (file.exists(path_3exp_vars) == TRUE){
      vars_a<-readRDS(system.file("extdata", "rb_mod3exp_vars.rds", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'rb_mod3exp' has been used for predictions, two files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'rb_mod3exp' for predictions.")
      download("rb_mod3exp_vars.rds",
               "https://osf.io/8ut7r/download")
      vars_a<-readRDS(system.file("extdata", "rb_mod3exp_vars.rds", package = "writeAlizer"))
    }
  }
  ##### rb_mod3per
  #check if the variable list exists, if so, load it; if not, download and load it
  if (model=="rb_mod3per") {
    path_3per_vars <- system.file("extdata", "rb_mod3per_vars.rds", package = "writeAlizer")
    if (file.exists(path_3per_vars) == TRUE){
      vars_a<-readRDS(system.file("extdata", "rb_mod3per_vars.rds", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'rb_mod3per' has been used for predictions, two files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'rb_mod3per' for predictions.")
      download("rb_mod3per_vars.rds",
               "https://osf.io/8mgcn/download")
      vars_a<-readRDS(system.file("extdata", "rb_mod3per_vars.rds", package = "writeAlizer"))
    }
  }
  ##### rb_mod3all
  #check if the variable list exists, if so, load it; if not, download and load it
  if (model=="rb_mod3all") {
    path_3narr_vars <- system.file("extdata", "rb_mod3narr_vars.rds", package = "writeAlizer")
    if (file.exists(path_3narr_vars) == TRUE){
      vars_a<-readRDS(system.file("extdata", "rb_mod3narr_vars.rds", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'rb_mod3all' has been used for predictions, six files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'rb_mod3all' for predictions.")
      download("rb_mod3narr_vars.rds",
               "https://osf.io/bmqg9/download")
      vars_a<-readRDS(system.file("extdata", "rb_mod3narr_vars.rds", package = "writeAlizer"))
    }
    path_3exp_vars <- system.file("extdata", "rb_mod3exp_vars.rds", package = "writeAlizer")
    if (file.exists(path_3exp_vars) == TRUE){
      vars_b<-readRDS(system.file("extdata", "rb_mod3exp_vars.rds", package = "writeAlizer"))
    } else {
      download("rb_mod3exp_vars.rds",
               "https://osf.io/8ut7r/download")
      vars_b<-readRDS(system.file("extdata", "rb_mod3exp_vars.rds", package = "writeAlizer"))
    }
    path_3per_vars <- system.file("extdata", "rb_mod3per_vars.rds", package = "writeAlizer")
    if (file.exists(path_3per_vars) == TRUE){
      vars_c<-readRDS(system.file("extdata", "rb_mod3per_vars.rds", package = "writeAlizer"))
    } else {
      download("rb_mod3per_vars.rds",
               "https://osf.io/8mgcn/download")
      vars_c<-readRDS(system.file("extdata", "rb_mod3per_vars.rds", package = "writeAlizer"))
    }
  }
  ##### rb_mod2
  #check if the variable list exists, if so, load it; if not, download and load it
  if (model=="rb_mod2") {
    path_2a_vars <- system.file("extdata", "rb_mod2a_vars.rds", package = "writeAlizer")
    if (file.exists(path_2a_vars) == TRUE){
      vars_a<-readRDS(system.file("extdata", "rb_mod2a_vars.rds", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'rb_mod2' has been used for predictions, six files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'rb_mod2' for predictions.")
      download("rb_mod2a_vars.rds",
               "https://osf.io/2rsnc/download")
      vars_a<-readRDS(system.file("extdata", "rb_mod2a_vars.rds", package = "writeAlizer"))
    }
    path_2b_vars <- system.file("extdata", "rb_mod2b_vars.rds", package = "writeAlizer")
    if (file.exists(path_2b_vars) == TRUE){
      vars_b<-readRDS(system.file("extdata", "rb_mod2b_vars.rds", package = "writeAlizer"))
    } else {
      download("rb_mod2b_vars.rds",
               "https://osf.io/qjg68/download")
      vars_b<-readRDS(system.file("extdata", "rb_mod2b_vars.rds", package = "writeAlizer"))
    }
    path_2c_vars <- system.file("extdata", "rb_mod2c_vars.rds", package = "writeAlizer")
    if (file.exists(path_2c_vars) == TRUE){
      vars_c<-readRDS(system.file("extdata", "rb_mod2c_vars.rds", package = "writeAlizer"))
    } else {
      download("rb_mod2c_vars.rds",
               "https://osf.io/kqdvt/download")
      vars_c<-readRDS(system.file("extdata", "rb_mod2c_vars.rds", package = "writeAlizer"))
    }
  }
  ##### coh_mod2
  if (model=="coh_mod2") {
    path_2a_vars <- system.file("extdata", "coh_mod2a_vars.rds", package = "writeAlizer")
    if (file.exists(path_2a_vars) == TRUE){
      vars_a<-readRDS(system.file("extdata", "coh_mod2a_vars.rds", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'coh_mod2' has been used for predictions, six files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'coh_mod2' for predictions.")
      download("coh_mod2a_vars.rds",
               "https://osf.io/qp7fc/download")
      vars_a<-readRDS(system.file("extdata", "coh_mod2a_vars.rds", package = "writeAlizer"))
    }
    path_2b_vars <- system.file("extdata", "coh_mod2b_vars.rds", package = "writeAlizer")
    if (file.exists(path_2b_vars) == TRUE){
      vars_b<-readRDS(system.file("extdata", "coh_mod2b_vars.rds", package = "writeAlizer"))
    } else {
      download("coh_mod2b_vars.rds",
               "https://osf.io/upn6j/download")
      vars_b<-readRDS(system.file("extdata", "coh_mod2b_vars.rds", package = "writeAlizer"))
    }
    path_2c_vars <- system.file("extdata", "coh_mod2c_vars.rds", package = "writeAlizer")
    if (file.exists(path_2c_vars) == TRUE){
      vars_c<-readRDS(system.file("extdata", "coh_mod2c_vars.rds", package = "writeAlizer"))
    } else {
      download("coh_mod2c_vars.rds",
               "https://osf.io/8qmzv/download")
      vars_c<-readRDS(system.file("extdata", "coh_mod2c_vars.rds", package = "writeAlizer"))
    }
  }
  ##### if model with no preprocessing, copy data to data_pp
  if (model=="rb_mod1" | model=="coh_mod1") {
    data_pp <- list(data, data, data, data, data, data)
  } else if (model=="gamet_cws1") {
    data_pp <- list(data)
  } else if (model=="rb_mod2" | model=="coh_mod2" | model=="rb_mod3all") {
  ##### select variables for each model, preprocess data, export in list
  data1 <- data %>% select(all_of(vars_a))
  pp1 <- preProcess(data1, method=c("center", "scale"))
  data1r <- predict(pp1, data1)
  data1pp <- data.frame(cbind(ID=data$ID, data1r))

  data2 <- data %>% select(all_of(vars_b))
  pp2 <- preProcess(data2, method=c("center", "scale"))
  data2r <- predict(pp2, data2)
  data2pp <- data.frame(cbind(ID=data$ID, data2r))

  data3 <- data %>% select(all_of(vars_c))
  pp3 <- preProcess(data3, method=c("center", "scale"))
  data3r <- predict(pp3, data3)
  data3pp <- data.frame(cbind(ID=data$ID, data3r))

  data_pp <- list(data1pp, data2pp, data3pp)
  } else if (model=="rb_mod3narr" | model=="rb_mod3exp" | model=="rb_mod3per") {
  ##### select variables for model, preprocess data, export in list
  data1 <- data %>% select(all_of(vars_a))
  pp1 <- preProcess(data1, method=c("center", "scale"))
  data1r <- predict(pp1, data1)
  data1pp <- data.frame(cbind(ID=data$ID, data1r))

  data_pp <- list(data1pp)
  }
  return(data_pp)
}

#' @title Download predictive models
#' @description Download models so that predicted quality scores can be generated
#' in the predict_quality function
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca>
#' @param model A string telling which scoring model to use.
#' Options are:
#' 'rb_mod1', 'rb_mod2', 'rb_mod3narr', 'rb_mod3exp',
#' 'rb_mod3per', or 'rb_mod3all' for ReaderBench files,
#' 'coh_mod1' and 'coh_mod2' for Coh-Metrix models, or
#' 'gamet_cws1' for GAMET files
#' @export
#' @seealso
#' \code{\link{predict_quality}}
#' @examples
#' #load package
#' library(writeAlizer)
#'
#' #Download ReaderBench Model 2
#' download_mod('rb_mod2')
download_mod <- function(model) {
  ##### rb_mod1
  if (model=='rb_mod1'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_1a <- system.file("extdata", "rb_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_1a) == TRUE){
      load(system.file("extdata", "rb_mod1a.rda", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'rb_mod1' has been used for predictions, six files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'rb_mod1' for predictions.")
      download("rb_mod1a.rda",
               "https://osf.io/eq9rw/download")
      load(system.file("extdata", "rb_mod1a.rda", package = "writeAlizer"))
    }
    path_1b <- system.file("extdata", "rb_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_1b) == TRUE){
      load(system.file("extdata", "rb_mod1b.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1b.rda",
               "https://osf.io/sy4dw/download")
      load(system.file("extdata", "rb_mod1b.rda", package = "writeAlizer"))
    }
    path_1c <- system.file("extdata", "rb_mod1c.rda", package = "writeAlizer")
    if (file.exists(path_1c) == TRUE){
      load(system.file("extdata", "rb_mod1c.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1c.rda",
               "https://osf.io/64dxf/download")
      load(system.file("extdata", "rb_mod1c.rda", package = "writeAlizer"))
    }
    path_1d <- system.file("extdata", "rb_mod1d.rda", package = "writeAlizer")
    if (file.exists(path_1d) == TRUE){
      load(system.file("extdata", "rb_mod1d.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1d.rda",
               "https://osf.io/5yghv/download")
      load(system.file("extdata", "rb_mod1d.rda", package = "writeAlizer"))
    }
    path_1e <- system.file("extdata", "rb_mod1e.rda", package = "writeAlizer")
    if (file.exists(path_1e) == TRUE){
      load(system.file("extdata", "rb_mod1e.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1e.rda",
               "https://osf.io/kgxtu/download")
      load(system.file("extdata", "rb_mod1e.rda", package = "writeAlizer"))
    }
    path_1f <- system.file("extdata", "rb_mod1f.rda", package = "writeAlizer")
    if (file.exists(path_1f) == TRUE){
      load(system.file("extdata", "rb_mod1f.rda", package = "writeAlizer"))
    } else {
      download("rb_mod1f.rda",
               "https://osf.io/5wdet/download")
      load(system.file("extdata", "rb_mod1f.rda", package = "writeAlizer"))
    }
    models <- list(rb_mod1a, rb_mod1b, rb_mod1c, rb_mod1d, rb_mod1e, rb_mod1f)
  }
  ##### rb_mod2
  if (model=='rb_mod2'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_2a <- system.file("extdata", "rb_mod2a.rda", package = "writeAlizer")
    if (file.exists(path_2a) == TRUE){
      load(system.file("extdata", "rb_mod2a.rda", package = "writeAlizer"))
    } else {
      download("rb_mod2a.rda",
               "https://osf.io/bpzhs/download")
      load(system.file("extdata", "rb_mod2a.rda", package = "writeAlizer"))
    }
    path_2b <- system.file("extdata", "rb_mod2b.rda", package = "writeAlizer")
    if (file.exists(path_2b) == TRUE){
      load(system.file("extdata", "rb_mod2b.rda", package = "writeAlizer"))
    } else {
      download("rb_mod2b.rda",
               "https://osf.io/vzkhn/download")
      load(system.file("extdata", "rb_mod2b.rda", package = "writeAlizer"))
    }
    path_2c <- system.file("extdata", "rb_mod2c.rda", package = "writeAlizer")
    if (file.exists(path_2c) == TRUE){
      load(system.file("extdata", "rb_mod2c.rda", package = "writeAlizer"))
    } else {
      download("rb_mod2c.rda",
               "https://osf.io/cqkrv/download")
      load(system.file("extdata", "rb_mod2c.rda", package = "writeAlizer"))
    }
    models <- list(rb_mod2a, rb_mod2b, rb_mod2c)
  }
  ##### rb_mod3narr
  if (model=='rb_mod3narr'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_3narr <- system.file("extdata", "rb_mod3narr.rda", package = "writeAlizer")
    if (file.exists(path_3narr) == TRUE){
      load(system.file("extdata", "rb_mod3narr.rda", package = "writeAlizer"))
    } else {
      download("rb_mod3narr.rda",
               "https://osf.io/f4nhu/download")
      load(system.file("extdata", "rb_mod3narr.rda", package = "writeAlizer"))
    }
    models <- list(rb_mod3narr)
  }
  ##### rb_mod3exp
  if (model=='rb_mod3exp'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_3exp <- system.file("extdata", "rb_mod3exp.rda", package = "writeAlizer")
    if (file.exists(path_3exp) == TRUE){
      load(system.file("extdata", "rb_mod3exp.rda", package = "writeAlizer"))
    } else {
      download("rb_mod3exp.rda",
               "https://osf.io/rx6aj/download")
      load(system.file("extdata", "rb_mod3exp.rda", package = "writeAlizer"))
    }
    models <- list(rb_mod3exp)
  }
  ##### rb_mod3per
  if (model=='rb_mod3per'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_3per <- system.file("extdata", "rb_mod3per.rda", package = "writeAlizer")
    if (file.exists(path_3per) == TRUE){
      load(system.file("extdata", "rb_mod3per.rda", package = "writeAlizer"))
    } else {
      download("rb_mod3per.rda",
               "https://osf.io/kqxte/download")
      load(system.file("extdata", "rb_mod3per.rda", package = "writeAlizer"))
    }
    models <- list(rb_mod3per)
  }
  ##### rb_mod3all
  if (model=='rb_mod3all'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_3narr <- system.file("extdata", "rb_mod3narr.rda", package = "writeAlizer")
    if (file.exists(path_3narr) == TRUE){
      load(system.file("extdata", "rb_mod3narr.rda", package = "writeAlizer"))
    } else {
      download("rb_mod3narr.rda",
               "https://osf.io/f4nhu/download")
      load(system.file("extdata", "rb_mod3narr.rda", package = "writeAlizer"))
    }
    path_3exp <- system.file("extdata", "rb_mod3exp.rda", package = "writeAlizer")
    if (file.exists(path_3exp) == TRUE){
      load(system.file("extdata", "rb_mod3exp.rda", package = "writeAlizer"))
    } else {
      download("rb_mod3exp.rda",
               "https://osf.io/rx6aj/download")
      load(system.file("extdata", "rb_mod3exp.rda", package = "writeAlizer"))
    }
    path_3per <- system.file("extdata", "rb_mod3per.rda", package = "writeAlizer")
    if (file.exists(path_3per) == TRUE){
      load(system.file("extdata", "rb_mod3per.rda", package = "writeAlizer"))
    } else {
      download("rb_mod3per.rda",
               "https://osf.io/kqxte/download")
      load(system.file("extdata", "rb_mod3per.rda", package = "writeAlizer"))
    }
    models <- list(rb_mod3narr, rb_mod3exp, rb_mod3per)
  }
  ##### coh_mod1
  if (model == 'coh_mod1'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_1a <- system.file("extdata", "coh_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_1a) == TRUE){
      load(system.file("extdata", "coh_mod1a.rda", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'coh_mod1' has been used for predictions, six files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'coh_mod1' for predictions.")
      download("coh_mod1a.rda",
               "https://osf.io/qws5x/download")
      load(system.file("extdata", "coh_mod1a.rda", package = "writeAlizer"))
    }
    path_1b <- system.file("extdata", "coh_mod1b.rda", package = "writeAlizer")
    if (file.exists(path_1b) == TRUE){
      load(system.file("extdata", "coh_mod1b.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1b.rda",
               "https://osf.io/rdmfw/download")
      load(system.file("extdata", "coh_mod1b.rda", package = "writeAlizer"))
    }
    path_1c <- system.file("extdata", "coh_mod1c.rda", package = "writeAlizer")
    if (file.exists(path_1c) == TRUE){
      load(system.file("extdata", "coh_mod1c.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1c.rda",
               "https://osf.io/dq2s9/download")
      load(system.file("extdata", "coh_mod1c.rda", package = "writeAlizer"))
    }
    path_1d <- system.file("extdata", "coh_mod1d.rda", package = "writeAlizer")
    if (file.exists(path_1d) == TRUE){
      load(system.file("extdata", "coh_mod1d.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1d.rda",
               "https://osf.io/be6qv/download")
      load(system.file("extdata", "coh_mod1d.rda", package = "writeAlizer"))
    }
    path_1e <- system.file("extdata", "coh_mod1e.rda", package = "writeAlizer")
    if (file.exists(path_1e) == TRUE){
      load(system.file("extdata", "coh_mod1e.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1e.rda",
               "https://osf.io/pv3gm/download")
      load(system.file("extdata", "coh_mod1e.rda", package = "writeAlizer"))
    }
    path_1f <- system.file("extdata", "coh_mod1f.rda", package = "writeAlizer")
    if (file.exists(path_1f) == TRUE){
      load(system.file("extdata", "coh_mod1f.rda", package = "writeAlizer"))
    } else {
      download("coh_mod1f.rda",
               "https://osf.io/myk6f/download")
      load(system.file("extdata", "coh_mod1f.rda", package = "writeAlizer"))
    }
    models <- list(coh_mod1a, coh_mod1b, coh_mod1c, coh_mod1d, coh_mod1e, coh_mod1f)
  }
  ##### coh_mod2
  if (model == 'coh_mod2'){
    #check if each model object exists, if so, load it; if not, download and load it
    path_2a <- system.file("extdata", "coh_mod2a.rda", package = "writeAlizer")
    if (file.exists(path_2a) == TRUE){
      load(system.file("extdata", "coh_mod2a.rda", package = "writeAlizer"))
    } else {
      download("coh_mod2a.rda",
               "https://osf.io/mr7kg/download")
      load(system.file("extdata", "coh_mod2a.rda", package = "writeAlizer"))
    }
    path_2b <- system.file("extdata", "coh_mod2b.rda", package = "writeAlizer")
    if (file.exists(path_2b) == TRUE){
      load(system.file("extdata", "coh_mod2b.rda", package = "writeAlizer"))
    } else {
      download("coh_mod2b.rda",
               "https://osf.io/zxhcu/download")
      load(system.file("extdata", "coh_mod2b.rda", package = "writeAlizer"))
    }
    path_2c <- system.file("extdata", "coh_mod2c.rda", package = "writeAlizer")
    if (file.exists(path_2c) == TRUE){
      load(system.file("extdata", "coh_mod2c.rda", package = "writeAlizer"))
    } else {
      download("coh_mod2c.rda",
               "https://osf.io/n6hqg/download")
      load(system.file("extdata", "coh_mod2c.rda", package = "writeAlizer"))
    }
    models <- list(coh_mod2a, coh_mod2b, coh_mod2c)
  }
  ##### gamet_cws1
  if (model == 'gamet_cws1') {
    #check if each model object exists, if so, load it; if not, download and load it
    path_cws1a <- system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_cws1a) == TRUE){
      load(system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer"))
    } else {
      print("Because this is the first time 'gamet_cws1' has been used for predictions, two files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use 'gamet_cws1' for predictions.")
      download("CWS_mod1a.rda",
               "https://osf.io/tfw95/download")
      load(system.file("extdata", "CWS_mod1a.rda", package = "writeAlizer"))
    }
    path_ciws1a <- system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer")
    if (file.exists(path_ciws1a) == TRUE){
      load(system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer"))
    } else {
      download("CIWS_mod1a.rda",
               "https://osf.io/yjuxn/download")
      load(system.file("extdata", "CIWS_mod1a.rda", package = "writeAlizer"))
    }
    models <- list(CWS_mod1a, CIWS_mod1a)
  }
  return(models)
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
#' 'rb_mod3per', or 'rb_mod3all' for ReaderBench files to generate holistic quality,
#' 'coh_mod1' or 'coh_mod2' for Coh-Metrix files to generate holistic quality,
#' and 'gamet_cws1' to generate Correct Word Sequences (CWS)
#' and Correct Minus Incorrect Word Sequences (CIWS) scores from a GAMET file.
#' @param data The name of the R object corresponding to the data file. The
#' \code{\link{import_gamet}}import_gamet(), \code{\link{import_coh}}import_coh(), or
#' \code{\link{import_rb}}import_rb()
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
#' rb_quality <- predict_quality('rb_mod2', rb_file, store = FALSE)
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
#' coh_quality <- predict_quality('coh_mod2', coh_file, store = TRUE, name = "output.csv")
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
#' gamet_CWS_CIWS <- predict_quality('gamet_cws1', gam_file, store = FALSE)
#'
#' #display quality scores
#' gamet_CWS_CIWS
predict_quality <- function(model, data, store = FALSE, name = "filename.csv") {
  #to get predict s3 method from caretEnsemble
  requireNamespace("caretEnsemble", quietly = TRUE)

  #preprocess data
  data_pp <- preprocess(model, data)

  #download and load models
  models <- download_mod(model)

  ####special code for gamet models
  if (model=='gamet_cws1') {
    predicted_tww <- data$word_count
    predicted_wsc <- data$word_count - data$misspelling

    predicted_cws <- predict(models[[1]], data)
    predicted_ciws <- predict(models[[2]], data)
    if (store == TRUE){
    data.2 <- cbind(predicted_tww, predicted_wsc,predicted_cws,predicted_ciws,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)
    }
    return(data.frame(cbind(ID=data$ID,predicted_tww, predicted_wsc, predicted_cws, predicted_ciws)))
  } else {
  #for all other models apply models to data sets in lists
  predictions <- list()

  for (i in seq_len(length(models))) {
    predictions[[i]] <- predict(models[[i]], data_pp[[i]])
  }
  predicted_quality <- colMeans(do.call(rbind,predictions))
  id_quality <- data.frame(cbind(ID=data$ID, predicted_quality))
  if (store == TRUE) {
    data.2 <- cbind(predicted_quality,data)
    write.table(data.2, file = name, sep = ",", row.names = FALSE)
  }
  return(id_quality)
  }
}

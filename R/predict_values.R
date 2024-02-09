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
  ####set values by model for later use
  if (model=="rb_mod1" | model=="coh_mod1"){
    mod_count <- 6
  }
  if (model=="gamet_cws1"){
    mod_count <- 2
  }
  if (model=="rb_mod2"){
    mod_count <- 3

    rds_name1 <- "rb_mod2a_vars.rds"
    rds_name2 <- "rb_mod2b_vars.rds"
    rds_name3 <- "rb_mod2c_vars.rds"

    rds_path1 <- "https://osf.io/2rsnc/download"
    rds_path2 <- "https://osf.io/qjg68/download"
    rds_path3 <- "https://osf.io/kqdvt/download"
  }
  if (model=="rb_mod3narr") {
    mod_count <- 1

    rds_name1 <- "rb_mod3narr_vars.rds"

    rds_path1 <- "https://osf.io/bmqg9/download"
  }
  if (model=="rb_mod3exp") {
    mod_count <- 1

    rds_name1 <- "rb_mod3exp_vars.rds"

    rds_path1 <- "https://osf.io/8ut7r/download"
  }
  if (model=="rb_mod3per") {
    mod_count <- 1

    rds_name1 <- "rb_mod3per_vars.rds"

    rds_path1 <- "https://osf.io/8mgcn/download"
  }
  if (model=="rb_mod3all"){
    mod_count <- 3

    rds_name1 <- "rb_mod3exp_vars.rds"
    rds_name2 <- "rb_mod3narr_vars.rds"
    rds_name3 <- "rb_mod3per_vars.rds"

    rds_path1 <- "https://osf.io/8ut7r/download"
    rds_path2 <- "https://osf.io/bmqg9/download"
    rds_path3 <- "https://osf.io/8mgcn/download"
  }
  if (model=="rb_mod3narr_v2") {
    mod_count <- 1

    rds_name1 <- "rb_narr_vars_v2.rds"

    rds_path1 <- "https://osf.io/8v6nz/download"
  }
  if (model=="rb_mod3exp_v2") {
    mod_count <- 1

    rds_name1 <- "rb_exp_vars_v2.rds"

    rds_path1 <- "https://osf.io/gvtyx/download"
  }
  if (model=="rb_mod3per_v2") {
    mod_count <- 1

    rds_name1 <- "rb_per_vars_v2.rds"

    rds_path1 <- "https://osf.io/7dhc6/download"
  }
  if (model=="rb_mod3all_v2"){
    mod_count <- 3

    rds_name1 <- "rb_exp_vars_v2.rds"
    rds_name2 <- "rb_narr_vars_v2.rds"
    rds_name3 <- "rb_per_vars_v2.rds"

    rds_path1 <- "https://osf.io/gvtyx/download"
    rds_path2 <- "https://osf.io/8v6nz/download"
    rds_path3 <- "https://osf.io/7dhc6/download"
  }
  if (model=="coh_mod2"){
    mod_count <- 3

    rds_name1 <- "coh_mod2a_vars.rds"
    rds_name2 <- "coh_mod2b_vars.rds"
    rds_name3 <- "coh_mod2c_vars.rds"

    rds_path1 <- "https://osf.io/qp7fc/download"
    rds_path2 <- "https://osf.io/upn6j/download"
    rds_path3 <- "https://osf.io/8qmzv/download"
  }
  if (model=="coh_mod3narr") {
    mod_count <- 1

    rds_name1 <- "coh_narr_vars.rds"

    rds_path1 <- "https://osf.io/rbg9n/download"
  }
  if (model=="coh_mod3exp") {
    mod_count <- 1

    rds_name1 <- "coh_exp_vars.rds"

    rds_path1 <- "https://osf.io/v5wf3/download"
  }
  if (model=="coh_mod3per") {
    mod_count <- 1

    rds_name1 <- "coh_per_vars.rds"

    rds_path1 <- "https://osf.io/ekrgu/download"
  }
  if (model=="coh_mod3all") {
    mod_count <- 3

    rds_name1 <- "coh_exp_vars.rds"
    rds_name2 <- "coh_narr_vars.rds"
    rds_name3 <- "coh_per_vars.rds"

    rds_path1 <- "https://osf.io/v5wf3/download"
    rds_path2 <- "https://osf.io/rbg9n/download"
    rds_path3 <- "https://osf.io/ekrgu/download"
  }
  #check if the variable list exists, if so, load it; if not, download and load it
  if (mod_count==1){
    path <- system.file("extdata", rds_name1, package = "writeAlizer")
    if (file.exists(path) == TRUE){
      vars_a<-readRDS(system.file("extdata", rds_name1, package = "writeAlizer"))
    } else {
      print("Because this is the first time this model has been used for predictions, two files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use this model for predictions.")
      download(rds_name1,
               rds_path1)
      vars_a<-readRDS(system.file("extdata", rds_name1, package = "writeAlizer"))
    }
  }
  if (mod_count==3){
    path <- system.file("extdata", rds_name1, package = "writeAlizer")
    if (file.exists(path) == TRUE){
      vars_a<-readRDS(system.file("extdata", rds_name1, package = "writeAlizer"))
    } else {
      print("Because this is the first time this model has been used for predictions, six files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use this model for predictions.")
      download(rds_name1,
               rds_path1)
      vars_a<-readRDS(system.file("extdata", rds_name1, package = "writeAlizer"))
    }
    path <- system.file("extdata", rds_name2, package = "writeAlizer")
    if (file.exists(path) == TRUE){
      vars_b<-readRDS(system.file("extdata", rds_name2, package = "writeAlizer"))
    } else {
      download(rds_name2,
               rds_path2)
      vars_b<-readRDS(system.file("extdata", rds_name2, package = "writeAlizer"))
    }
    path <- system.file("extdata", rds_name3, package = "writeAlizer")
    if (file.exists(path) == TRUE){
      vars_c<-readRDS(system.file("extdata", rds_name3, package = "writeAlizer"))
    } else {
      download(rds_name3,
               rds_path3)
      vars_c<-readRDS(system.file("extdata", rds_name3, package = "writeAlizer"))
    }
  }
  ##### if model with no preprocessing, copy data to data_pp
  if (model=="rb_mod1" | model=="coh_mod1") {
    data_pp <- list(data, data, data, data, data, data)
  } else if (model=="gamet_cws1") {
    data_pp <- list(data)
  } else if (model=="rb_mod2" | model=="coh_mod2" | model=="rb_mod3all" |
             model == "rb_mod3all_v2" | model == "coh_mod3all") {
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
  } else if (model=="rb_mod3narr" | model=="rb_mod3exp" | model=="rb_mod3per" |
             model=="coh_mod3narr" | model=="coh_mod3exp" | model=="coh_mod3per" |
             model=="rb_mod3narr_v2" | model=="rb_mod3exp_v2" |
             model=="rb_mod3per_v2") {
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
#' 'rb_mod3per', 'rb_mod3all', 'rb_mod3narr_v2', 'rb_mod3exp_v2',
#' 'rb_mod3per_v2', or 'rb_mod3all_v2' for ReaderBench files,
#' 'coh_mod1', 'coh_mod2', 'coh_mod3narr', 'coh_mod3exp', 'coh_mod3per'
#'  or 'coh_mod3all' for Coh-Metrix files, or
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
    #set values for later use
    mod_count <- 6

    rda_name1 <- "rb_mod1a.rda"
    rda_name2 <- "rb_mod1b.rda"
    rda_name3 <- "rb_mod1c.rda"
    rda_name4 <- "rb_mod1d.rda"
    rda_name5 <- "rb_mod1e.rda"
    rda_name6 <- "rb_mod1f.rda"

    rda_path1 <- "https://osf.io/eq9rw/download"
    rda_path2 <- "https://osf.io/sy4dw/download"
    rda_path3 <- "https://osf.io/64dxf/download"
    rda_path4 <- "https://osf.io/5yghv/download"
    rda_path5 <- "https://osf.io/kgxtu/download"
    rda_path6 <- "https://osf.io/5wdet/download"
  }
  ##### rb_mod2
  if (model=='rb_mod2'){
    #set values for later use
    mod_count <- 3

    rda_name1 <- "rb_mod2a.rda"
    rda_name2 <- "rb_mod2b.rda"
    rda_name3 <- "rb_mod2c.rda"

    rda_path1 <- "https://osf.io/bpzhs/download"
    rda_path2 <- "https://osf.io/vzkhn/download"
    rda_path3 <- "https://osf.io/cqkrv/download"
  }
  ##### rb_mod3narr
  if (model=='rb_mod3narr'){
    #set values for later use
    mod_count <- 1

    rda_name1 <- "rb_mod3narr.rda"

    rda_path1 <- "https://osf.io/f4nhu/download"
  }
  ##### rb_mod3exp
  if (model=='rb_mod3exp'){
    #create new environment for loading models
    e <- new.env()

    #set values for later use
    mod_count <-1

    rda_name1 <- "rb_mod3exp.rda"

    rda_path1 <- "https://osf.io/rx6aj/download"
  }
  ##### rb_mod3per
  if (model=='rb_mod3per'){
    #create new environment for loading models
    e <- new.env()

    #set values for later use
    mod_count <- 1

    rda_name1 <- "rb_mod3per.rda"

    rda_path1 <- "https://osf.io/kqxte/download"
  }
  ##### rb_mod3all
  if (model=='rb_mod3all'){
    #create new environment for loading models
    e <- new.env()

    #set values for later use
    mod_count <- 3

    rda_name1 <- "rb_mod3exp.rda"
    rda_name2 <- "rb_mod3narr.rda"
    rda_name3 <- "rb_mod3per.rda"

    rda_path1 <- "https://osf.io/rx6aj/download"
    rda_path2 <- "https://osf.io/f4nhu/download"
    rda_path3 <- "https://osf.io/kqxte/download"
  }
  ##### rb_mod3narr_v2
  if (model=='rb_mod3narr_v2'){
    #set values for later use
    mod_count <- 1

    rda_name1 <- "rb_mod3narr_v2.rda"

    rda_path1 <- "https://osf.io/rqtzm/download"
  }
  ##### rb_mod3exp_v2
  if (model=='rb_mod3exp_v2'){
    #set values for later use
    mod_count <- 1

    rda_name1 <- "rb_mod3exp_v2.rda"

    rda_path1 <- "https://osf.io/hknxf/download"
  }
  ##### rb_mod3per_v2
  if (model=='rb_mod3per_v2'){
    #set values for later use
    mod_count <- 1

    rda_name1 <- "rb_mod3per_v2.rda"

    rda_path1 <- "https://osf.io/ntgfm/download"
  }
  ##### rb_mod3all_v2
  if (model=='rb_mod3all_v2'){
    #create new environment for loading models
    e <- new.env()

    #set values for later use
    mod_count <- 3

    rda_name1 <- "rb_mod3exp_v2.rda"
    rda_name2 <- "rb_mod3narr_v2.rda"
    rda_name3 <- "rb_mod3per_v2.rda"

    rda_path1 <- "https://osf.io/hknxf/download"
    rda_path2 <- "https://osf.io/rqtzm/download"
    rda_path3 <- "https://osf.io/ntgfm/download"
  }
  ##### coh_mod1
  if (model == 'coh_mod1'){
    #set values for later use
    mod_count <- 6

    rda_name1 <- "coh_mod1a.rda"
    rda_name2 <- "coh_mod1b.rda"
    rda_name3 <- "coh_mod1c.rda"
    rda_name4 <- "coh_mod1d.rda"
    rda_name5 <- "coh_mod1e.rda"
    rda_name6 <- "coh_mod1f.rda"

    rda_path1 <- "https://osf.io/qws5x/download"
    rda_path2 <- "https://osf.io/rdmfw/download"
    rda_path3 <- "https://osf.io/dq2s9/download"
    rda_path4 <- "https://osf.io/be6qv/download"
    rda_path5 <- "https://osf.io/pv3gm/download"
    rda_path6 <- "https://osf.io/myk6f/download"
  }
  ##### coh_mod2
  if (model == 'coh_mod2'){
    #set values for later use
    mod_count <- 3

    rda_name1 <- "coh_mod2a.rda"
    rda_name2 <- "coh_mod2b.rda"
    rda_name3 <- "coh_mod2c.rda"

    rda_path1 <- "https://osf.io/mr7kg/download"
    rda_path2 <- "https://osf.io/zxhcu/download"
    rda_path3 <- "https://osf.io/n6hqg/download"
  }
  ##### coh_mod3narr
  if (model=='coh_mod3narr'){
    #set values for later use
    mod_count <- 1

    rda_name1 <- "coh_mod3narr.rda"

    rda_path1 <- "https://osf.io/y5hjz/download"
  }
  ##### coh_mod3exp
  if (model=='coh_mod3exp'){
    #create new environment for loading models
    e <- new.env()

    #set values for later use
    mod_count <-1

    rda_name1 <- "coh_mod3exp.rda"

    rda_path1 <- "https://osf.io/6x95q/download"
  }
  ##### coh_mod3per
  if (model=='coh_mod3per'){
    #create new environment for loading models
    e <- new.env()

    #set values for later use
    mod_count <- 1

    rda_name1 <- "coh_mod3per.rda"

    rda_path1 <- "https://osf.io/vrnt9/download"
  }
  ##### coh_mod3all
  if (model=='coh_mod3all'){
    #create new environment for loading models
    e <- new.env()

    #set values for later use
    mod_count <- 3

    rda_name1 <- "coh_mod3exp.rda"
    rda_name2 <- "coh_mod3narr.rda"
    rda_name3 <- "coh_mod3per.rda"

    rda_path1 <- "https://osf.io/6x95q/download"
    rda_path2 <- "https://osf.io/y5hjz/download"
    rda_path3 <- "https://osf.io/vrnt9/download"
  }
  ##### gamet_cws1
  if (model == 'gamet_cws1') {
    #set values for later use
    mod_count <- 2

    rda_name1 <- "CWS_mod1a.rda"
    rda_name2 <- "CIWS_mod1a.rda"

    rda_path1 <- "https://osf.io/tfw95/download"
    rda_path2 <- "https://osf.io/yjuxn/download"
  }
  if (mod_count == 1){
    #create new environment for loading models
    e <- new.env()
    #check if each model object exists, if so, load it; if not, download and load it
    path1 <- system.file("extdata", rda_name1, package = "writeAlizer")
    if (file.exists(path1) == TRUE){
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name1,
               rda_path1)
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    }
    models <- as.list(e)
  }
  if (mod_count == 2){
    #create new environment for loading models
    e <- new.env()
    #check if each model object exists, if so, load it; if not, download and load it
    path1 <- system.file("extdata", rda_name1, package = "writeAlizer")
    if (file.exists(path1) == TRUE){
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    } else {
      print("Because this is the first time this model has been used for predictions, two files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use this model for predictions.")
      download(rda_name1,
               rda_path1)
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    }
    path2 <- system.file("extdata", rda_name2, package = "writeAlizer")
    if (file.exists(path2) == TRUE){
      load(system.file("extdata", rda_name2, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name2,
               rda_path2)
      load(system.file("extdata", rda_name2, package = "writeAlizer"), envir = e)
    }
    lst <- as.list(e)
    models <- lst[order(names(lst))]
  }
  if (mod_count == 3){
    #create new environment for loading models
    e <- new.env()
    #check if each model object exists, if so, load it; if not, download and load it
    path1 <- system.file("extdata", rda_name1, package = "writeAlizer")
    if (file.exists(path1) == TRUE){
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name1,
               rda_path1)
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    }
    path2 <- system.file("extdata", rda_name2, package = "writeAlizer")
    if (file.exists(path2) == TRUE){
      load(system.file("extdata", rda_name2, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name2,
               rda_path2)
      load(system.file("extdata", rda_name2, package = "writeAlizer"), envir = e)
    }
    path3 <- system.file("extdata", rda_name3, package = "writeAlizer")
    if (file.exists(path3) == TRUE){
      load(system.file("extdata", rda_name3, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name3,
               rda_path3)
      load(system.file("extdata", rda_name3, package = "writeAlizer"), envir = e)
    }
    lst <- as.list(e)
    models <- lst[order(names(lst))]
  }
  if (mod_count == 6){
    #create new environment for loading models
    e <- new.env()
    #check if each model object exists, if so, load it; if not, download and load it
    path1 <- system.file("extdata", rda_name1, package = "writeAlizer")
    if (file.exists(path1) == TRUE){
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    } else {
      print("Because this is the first time this model has been used for predictions, six files will be downloaded and stored in the 'extdata' folder of the writeAlizer R package directory. These files will not need to be downloaded the next time you use this model for predictions.")
      download(rda_name1,
               rda_path1)
      load(system.file("extdata", rda_name1, package = "writeAlizer"), envir = e)
    }
    path2 <- system.file("extdata", rda_name2, package = "writeAlizer")
    if (file.exists(path2) == TRUE){
      load(system.file("extdata", rda_name2, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name2,
               rda_path2)
      load(system.file("extdata", rda_name2, package = "writeAlizer"), envir = e)
    }
    path3 <- system.file("extdata", rda_name3, package = "writeAlizer")
    if (file.exists(path3) == TRUE){
      load(system.file("extdata", rda_name3, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name3,
               rda_path3)
      load(system.file("extdata", rda_name3, package = "writeAlizer"), envir = e)
    }
    path4 <- system.file("extdata", rda_name4, package = "writeAlizer")
    if (file.exists(path4) == TRUE){
      load(system.file("extdata", rda_name4, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name4,
               rda_path4)
      load(system.file("extdata", rda_name4, package = "writeAlizer"), envir = e)
    }
    path5 <- system.file("extdata", rda_name5, package = "writeAlizer")
    if (file.exists(path5) == TRUE){
      load(system.file("extdata", rda_name5, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name5,
               rda_path5)
      load(system.file("extdata", rda_name5, package = "writeAlizer"), envir = e)
    }
    path6 <- system.file("extdata", rda_name6, package = "writeAlizer")
    if (file.exists(path6) == TRUE){
      load(system.file("extdata", rda_name6, package = "writeAlizer"), envir = e)
    } else {
      download(rda_name6,
               rda_path6)
      load(system.file("extdata", rda_name6, package = "writeAlizer"), envir = e)
    }
    lst <- as.list(e)
    models <- lst[order(names(lst))]
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
#' #return scores to an object called "coh_quality",
#' coh_quality <- predict_quality('coh_mod2', coh_file, store = FALSE)
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

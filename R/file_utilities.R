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
# This file includes functions to import and pre-process Coh-Metrix, ReaderBench,
# and GAMET output files

#' Import a GAMET output file into R.
#'
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr mutate_all
#' @param path A string giving the path and filename to import.
#' @export
#' @seealso
#' \code{\link{predict_quality}}
#' @examples
#' ##Example 1:
#' #Using a sample data file included with writeAlizer package
#'
#' #load package
#' library(writeAlizer)
#'
#' #get path of sample GAMET output file
#' file_path <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
#'
#' #see path to sample file
#' file_path
#'
#' #import file and store as "gamet_file"
#' gamet_file <- import_gamet(file_path)
#'
#' ##Example 2:
#' #To import as "gamet_file" a GAMET file (sample name: gamet_output.csv)
#' #that is stored in the working directory
#' \dontrun{
#' gamet_file <- import_gamet('gamet_output.csv')
#' }
import_gamet <- function(path) {
  dat1<-read.csv(path, header = T)
  #strip filename from path
  dat1$filename<-basename(tools::file_path_sans_ext(dat1$filename))
  #rename filename variable
  names(dat1)[names(dat1) == "filename"] <- "ID"
  #make any factors numeric and sort by ID
  dat2<-mutate_all(dat1, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else if (is.character(x)) as.numeric(x) else x
  })
  dat3 <- dat2[order(dat2$ID),]
  dat4 <- dat3[,c("ID", "error_count", "word_count", "grammar", "misspelling")]
  dat4$per_gram <- dat4$grammar/dat4$word_count
  dat4$per_spell <- dat4$misspelling/dat4$word_count
  return(dat4)
}

#' Import a Coh-Metrix output file(.csv) into R.
#'
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr mutate_all
#' @param path A string giving the path and filename to import.
#' @export
#' @seealso
#' \code{\link{predict_quality}}
#' @examples
#' ##Example 1:
#' #Using a sample data file included with writeAlizer package
#'
#' #load package
#' library(writeAlizer)
#'
#' #get path of sample Coh-Metrix output file
#' file_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
#'
#' #see path to sample file
#' file_path
#'
#' #import file and store as "coh_file"
#' coh_file <- import_coh(file_path)
#'
#' ##Example 2:
#' #To import as 'coh_file' a Coh-Metrix file (sample name: coh_output.csv)
#' #that is stored in the working directory
#' \dontrun{
#' coh_file <- import_coh("coh_output.csv")
#' }
import_coh <- function(path) {
  dat1<-read.csv(path, header = T)
  #strip filename from path
  dat1$TextID<-basename(tools::file_path_sans_ext(dat1$TextID))
  #rename TextID variable
  names(dat1)[names(dat1) == "TextID"] <- "ID"
  #make any factors numeric and sort by ID
  dat2<-mutate_all(dat1, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else if (is.character(x)) as.numeric(x) else x
  })
  dat3 <- dat2[order(dat2$ID),]
  return(dat3)
}

#' Import a ReaderBench output file(.csv) into R.
#'
#' @importFrom magrittr %>%
#' @importFrom utils modifyList read.table
#' @importFrom dplyr na_if
#' @export
#' @seealso
#' \code{\link{predict_quality}}
#' @param path A string giving the path and filename to import.
#' @examples
#' #' ##Example 1:
#' #Using a sample data file included with writeAlizer package
#'
#' #load package
#' library(writeAlizer)
#'
#' #get path of sample ReaderBench output file
#' file_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
#'
#' #see path to sample file
#' file_path
#'
#' #import file and store as "rb_file"
#' rb_file <- import_rb(file_path)
#'
#' ##Example 2:
#' #To import as "rb_file" a ReaderBench file (sample name: rb_output.csv)
#' #that is stored in the working directory
#' \dontrun{
#' rb_file <- import_rb("rb_output.csv")
#' }
import_rb <- function(path) {
  #check first line for "SEP=,"; if there, exclude line during import
  con <- file(path,"r")
  first_line <- readLines(con,n=1)
  close(con)

  if (first_line=="SEP=,"){
    dat_RB<-read.table(
      text = readLines(path, warn = FALSE),
      header = TRUE,
      sep = ",", skip=1
    )
  }
  if (first_line!="SEP=,"){
    dat_RB<-read.table(
      text = readLines(path, warn = FALSE),
      header = TRUE,
      sep = ",")
  }
  dat_RB <- dat_RB %>% na_if("NaN")
  dat_RB2<-dat_RB[,1:404] #exclude the sentiment analysis colums
  names(dat_RB2)[names(dat_RB2)=="File.name"]<-"ID"
  #make any factors numeric and sort by ID
  dat_RB3<-mutate_all(dat_RB2, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else if (is.character(x)) as.numeric(x) else x
  })
  dat_RB4 <- dat_RB3[order(dat_RB3$ID),]
  return(dat_RB4)
}

#' Import a ReaderBench output file(.csv) and GAMET output file (.csv) into R, and merge the two files.
#'
#' @importFrom magrittr %>%
#' @importFrom utils modifyList read.csv read.table
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr na_if mutate_all
#' @export
#' @seealso
#' \code{\link{predict_quality}}
#' @param rb_path A string giving the path and ReaderBench filename to import.
#' @param gamet_path A string giving the path and GAMET filename to import.
#' @examples
#' ##Example 1:
#' #Using a sample data files included with writeAlizer package
#'
#' #load package
#' library(writeAlizer)
#'
#' #get path of sample ReaderBench output file
#' file_path1 <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
#'
#' #see path to sample ReaderBench file
#' file_path1
#'
#' #get path of sample GAMET output file
#' file_path2 <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
#'
#' #see path to sample GAMET file
#' file_path2
#'
#' #import files, merge, and store as "rb_gam_file"
#' rb_gam_file <- import_merge_gamet_rb(file_path1, file_path2)
#'
#' ##Example 2:
#' #To import as "rb_gam_file" a ReaderBench file (sample name: rb_output.csv)
#' #and GAMET file (sample name: gamet_output.csv) stored in the working
#' #directory and then merge them
#' \dontrun{
#' rb_gam_file <- import_merge_gamet_rb("rb_output.csv", "gamet_output.csv")
#' }
import_merge_gamet_rb <- function(rb_path, gamet_path) {
  #import RB
  dat.RB <- import_rb(rb_path)
  #import GAMET
  dat.G <- import_gamet(gamet_path)
  #merge RB and GAMET
  merge(dat.G, dat.RB, by.x="ID", by.y="ID")
}

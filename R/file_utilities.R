# This file includes functions to load model objects to generate predictions
# and to pre-process Coh-Metrix, ReaderBench, and GAMET input files

#' @title Download scoring models to the working directory.
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca>
#' @importFrom utils download.file
#' @export
#' @param mod A string telling which model group to download. Options are
#' 'rb_mod1' for ReaderBench files to generate holistic quality,
#' 'coh_mod1' for Coh-Metrix files to generate holistic quality,
#' and 'rb_gamet_cws1' for a merged ReaderBench and GAMET file
#' to generate Correct Word Sequences (CWS) and Correct Minus Incorrect
#' Word Sequences (CIWS) scores
#' @examples
#' #To download models to process ReaderBench files:
#' \dontrun{
#' download_model('rb_mod1')
#' }
#' #To download models to process Coh-Metrix files:
#' \dontrun{
#' download_model('coh_mod1')
#' }
#' #To download models to generate CWS/CIWS scores from ReaderBench and GAMET files:
#' \dontrun{
#' download_model('rb_gamet_cws1')
#' }
download_model <- function(mod) {
  if (mod == 'rb_mod1') {
    download.file(url="https://www.dropbox.com/s/yhz7uld7ifpy5na/rb_ensemble_fall7min_all.rds?dl=1",
                  destfile="./rb_mod1a.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/jnzkhkylrps74db/rb_ensemble_win7min_all.rds?dl=1",
                  destfile="./rb_mod1b.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/m98j8j122m5kazw/rb_ensemble_spr7min_all.rds?dl=1",
                  destfile="./rb_mod1c.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/rreoqqip7w60vwe/rb_ensemble_fall7min_pca.rds?dl=1",
                  destfile="./rb_mod1d.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/8ir43kfttgtv814/rb_ensemble_win7min_pca.rds?dl=1",
                  destfile="./rb_mod1e.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/te2e9mwyywdvs5u/rb_ensemble_spr7min_pca.rds?dl=1",
                  destfile="./rb_mod1f.rds", mode = "wb")
  }  else if (mod == 'coh_mod1') {
    download.file(url="https://www.dropbox.com/s/mq2nse1717ik871/coh_ensemble_fall7min_all.rds?dl=1",
                  destfile="./coh_mod1a.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/hbocz18y8sjkb2g/coh_ensemble_win7min_all.rds?dl=1",
                  destfile="./coh_mod1b.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/f4hn59js6ed159e/coh_ensemble_spr7min_all.rds?dl=1",
                  destfile="./coh_mod1c.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/uk60a3zecaelwqk/coh_ensemble_fall7min_pca.rds?dl=1",
                  destfile="./coh_mod1d.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/v3pmoptyymnr96r/coh_ensemble_win7min_pca.rds?dl=1",
                  destfile="./coh_mod1e.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/dczd34qfvbecli9/coh_ensemble_spr7min_pca.rds?dl=1",
                  destfile="./coh_mod1f.rds", mode = "wb")
  } else if (mod == 'rb_gamet_cws1') {
    download.file(url="https://www.dropbox.com/s/9qjbnp47g7uij0v/CIWS_ensemble_all7min.rds?dl=1",
                  destfile="./CIWS_mod1a.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/u9fp961t5xsf28l/CIWS_pca_ensemble_all7min.rds?dl=1",
                  destfile="./CIWS_mod1b.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/v88pfekdnxr3cox/CWS_ensemble_all7min.rds?dl=1",
                  destfile="./CWS_mod1a.rds", mode = "wb")
    download.file(url="https://www.dropbox.com/s/baipp2z1nloooid/CWS_pca_ensemble_all7min.rds?dl=1",
                  destfile="./CWS_mod1b.rds", mode = "wb")
  } else {
    print("Not a valid model name")
  }
}

#' Import a GAMET output file into R.
#'
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca> and Xuejun Ji
#' @importFrom stats setNames
#' @importFrom utils read.csv tail
#' @param path A string giving the path and filename to import.
#' @export
#' @examples
#' #To import as 'gamet_file' a GAMET file (sample name: gamet_output.csv)
#' # that is stored in the working directory
#' \dontrun{
#' gamet_file <- import_gamet('gamet_output.csv')
#' }
import_gamet <- function(path) {
  #message
  print('Multiple instances of this error message are normal: number of items to replace is not a multiple of replacement length')

  #import GAMET
  dat1<-read.csv(path, header = T)
  col<-as.vector(dat1$filename)
  unit<-list()
  for(i in 1:length(col)) {
    ## parsing the file path into units
    ## select the last unint in the list
    unit[i]<-tail(unlist(strsplit(col[i], "[\\,.]")),2)
  }
  temp<-data.table::transpose(unit) # transpose the list to vector
  temp<-setNames(data.frame(temp),'ID')
  #convert into dataframe and rename it
  dat1_merge<-cbind(temp,dat1[-1])
  dat1_merge_r<-dplyr::mutate_all(dat1_merge, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  dat2 <- dat1_merge_r[,1:5]
  dat3 <- dat2[order(dat2$ID),]
  return(dat3)
}

#' Import a Coh-Metrix output file(.csv) into R.
#'
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca> and Xuejun Ji
#' @importFrom stats setNames
#' @importFrom utils read.csv tail
#' @param path A string giving the path and filename to import.
#' @export
#' @examples
#' #To import as 'coh_file' a Coh-Metrix file (sample name: coh_output.csv)
#' #that is stored in the working directory
#' \dontrun{
#' coh_file <- import_coh('coh_output.csv')
#' }
import_coh <- function(path) {
  dat1<-read.csv(path, header = T)
  #TextID is field in Coh file
  col<-as.vector(dat1$TextID)
  unit<-list()
  for(i in 1:length(col)) {
    ## parsing the file path into units
    ## select the last unint in the list
    unit[i]<-tail(unlist(strsplit(col[i], "[\\,.]")),2)
  }
  temp<-data.table::transpose(unit) # transpose the list to vector
  temp<-setNames(data.frame(temp),'ID')
  #convert into dataframe and rename it
  dat1_merge<-cbind(temp,dat1[-1])
  dat1_merge_r<-dplyr::mutate_all(dat1_merge, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  dat2 <- dat1_merge_r[order(dat1_merge_r$ID),]
  return(dat2)
}

#' Import a ReaderBench output file(.xlsx) into R. ReaderBench output files (.csv)
#' should be converted to Excel format (.xlsx) before import.
#'
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca> and Xuejun Ji
#' @importFrom magrittr %>%
#' @importFrom utils modifyList
#' @export
#' @param path A string giving the path and filename to import.
#' @examples
#' #To import as 'rb_file' a ReaderBench file (sample name: rb_output.xlsx)
#' #that is stored in the working directory
#' \dontrun{
#' rb_file <- import_rb('rb_output.xlsx')
#' }
import_rb <- function(path) {
  dat_RB<-xlsx::read.xlsx(path, header = T,1)
  dat_RB <- dat_RB %>% dplyr::na_if("NaN")
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],
                                                     asNumeric))
  dat_RB<-factorsNumeric(dat_RB)
  dat_RB2<-dat_RB[,1:404] #exclude the sentiment analysis colums
  names(dat_RB2)[names(dat_RB2)=="File.name"]<-"ID"
  dat_RB3 <- dat_RB2[order(dat_RB2$ID),]
  return(dat_RB3)
}

#' Import a ReaderBench output file(.xlsx) and GAMET output file (.csv) into R, and merge the two files.
#' ReaderBench output files (.csv) should be converted to Excel format (.xlsx) before import.
#'
#' @author Sterett H. Mercer <sterett.mercer@@ubc.ca> and Xuejun Ji
#' @importFrom magrittr %>%
#' @importFrom utils modifyList read.csv tail
#' @importFrom stats setNames
#' @export
#' @param rb_path A string giving the path and ReaderBench filename to import.
#' @param gamet_path A string giving the path and GAMET filename to import.
#' @examples
#' #To import as 'rb_gam_file' a ReaderBench file (sample name: rb_output.xlsx)
#' #and GAMET file (sample name: gamet_output.csv) stored in the working
#' #directory and then merge them
#' \dontrun{
#' rb_gam_file <- import_merge_gamet_rb('rb_output.xlsx', 'gamet_output.csv')
#' }
import_merge_gamet_rb <- function(rb_path, gamet_path) {
  #message
  print('Multiple instances of this error message are normal: number of items to replace is not a multiple of replacement length')

  #import RB data
  dat_RB<-xlsx::read.xlsx(rb_path, header = T,1)
  dat_RB <- dat_RB %>% dplyr::na_if("NaN")
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],
                                                     asNumeric))
  dat_RB<-factorsNumeric(dat_RB)
  dat_RB2<-dat_RB[,1:404] #exclude the sentiment analysis colums
  names(dat_RB2)[names(dat_RB2)=="File.name"]<-"ID"
  dat_RB3 <- dat_RB2[order(dat_RB2$ID),]

  #import GAMET data
  datG<-read.csv(gamet_path, header = T)
  col<-as.vector(datG$filename)
  unit<-list()
  for(i in 1:length(col)) {
    ## parsing the file path into units
    ## select the last unint in the list
    unit[i]<-tail(unlist(strsplit(col[i], "[\\,.]")),2)
  }
  temp<-data.table::transpose(unit) # transpose the list to vector
  temp<-setNames(data.frame(temp),'ID')
  #convert into dataframe and rename it
  datG_merge<-cbind(temp,datG[-1])
  datG_merge_r<-dplyr::mutate_all(datG_merge, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  datG2 <- datG_merge_r[,1:5]
  datG2$per_gram <- datG2$grammar/datG2$word_count
  datG2$per_spell <- datG2$misspelling/datG2$word_count
  datG3 <- datG2[order(datG2$ID),]

  #merge RB and GAMET
  merge(datG3, dat_RB3, by.x="ID", by.y="ID")
}

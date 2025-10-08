# This file provides documentation for the writeAlizer package

#' writeAlizer: An R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores.
#'
#' @description Package-level documentation for writeAlizer.
#'
#' @details
#' Detailed documentation on writeAlizer is available in the
#' \href{https://github.com/shmercer/writeAlizer}{GitHub README file}
#' and \href{https://github.com/shmercer/writeAlizer/wiki}{wiki}.
#'
#' The writeAlizer R package (a) imports \href{https://github.com/readerbench/readerbench-java}{ReaderBench},
#' \href{https://soletlab.asu.edu/coh-metrix/}{Coh-Metrix}, and
#' \href{https://www.linguisticanalysistools.org/gamet.html}{GAMET} output files into R, and
#' (b) uses research-developed scoring models to generate predicted writing quality scores
#' or Correct Word Sequences and Correct Minus Incorrect Word Sequences scores from those files.
#'
#' The writeAlizer package includes functions to do two types of tasks:
#' (1) importing ReaderBench, Coh-Metrix, and/or GAMET output files into R; and
#' (2) generating predicted quality scores using the imported output files.
#' There are also additional functions to help with (3) installation of package dependencies and
#' (4) cache management.
#'
#' @aliases writeAlizer
#'
#' @section 1. Import output files:
#' \itemize{
#'   \item \code{\link{import_rb}}
#'   \item \code{\link{import_coh}}
#'   \item \code{\link{import_gamet}}
#'   \item \code{\link{import_merge_gamet_rb}}
#' }
#'
#' @section 2. Generate predicted quality scores:
#' \itemize{
#'   \item \code{\link{predict_quality}}
#' }
#'
#' @section 3. Identify necessary packages:
#' \itemize{
#'   \item \code{\link{model_deps}}
#' }
#'
#' @section 4. Cache management:
#' \itemize{
#'   \item \code{\link{wa_cache_dir}}
#'   \item \code{\link{wa_cache_clear}}
#' }
#'
#' @importFrom glue glue_collapse
"_PACKAGE"

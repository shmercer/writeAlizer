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
# This file provides documentation for the writeAlizer package

#' writeAlizer: An R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores.
#'
#' Detailed documentation on writeAlizer is available in the \href{https://github.com/shmercer/writeAlizer}{GitHub README file}
#' and \href{https://github.com/shmercer/writeAlizer/wiki}{wiki}
#'
#' The writeAlizer R package (a) imports \href{https://git.readerbench.com/ReaderBench/ReaderBench}{ReaderBench},
#' \href{http://cohmetrix.com/}{Coh-Metrix}, and \href{https://www.linguisticanalysistools.org/gamet.html}{GAMET} output files into R,
#' and (b) uses research-developed scoring models to generate predicted writing
#' quality scores or Correct Word Sequences and Correct Minus Incorrect Word Sequences
#' scores from the ReaderBench, Coh-Metrix, and/or GAMET files.
#'
#' The writeAlizer package includes functions to do two types of tasks:
#' (1) importing ReaderBench, Coh-Metrix, and/or GAMET output files into R;
#' and (2) generating predicted quality scores using the imported output files.
#'
#' @section 1. Import output files:
#' \code{\link{import_rb}}
#'
#' \code{\link{import_coh}}
#'
#' \code{\link{import_gamet}}
#'
#' \code{\link{import_merge_gamet_rb}}
#'
#' @section 2. Generate predicted quality scores:
#' \code{\link{predict_quality}}
#'
#' @docType package
#' @name writeAlizer
NULL

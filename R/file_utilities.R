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

# ---- helpers --------------------------------------------------------------

# Treats strings like "12", "-3.5", "1e-3" (with optional whitespace) as numeric-like
.wa_num_like <- function(x) {
  is.character(x) &&
    all(is.na(x) | grepl("^\\s*[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?\\s*$", x, perl = TRUE))
}

# Convert numeric-like character columns to numeric, excluding specific columns (e.g., "ID")
.wa_convert_numeric_like <- function(df, exclude = character()) {
  cols <- setdiff(names(df), exclude)
  to_num <- cols[vapply(df[cols], .wa_num_like, logical(1))]
  if (length(to_num)) {
    df[to_num] <- lapply(df[to_num], function(x) suppressWarnings(as.numeric(x)))
  }
  df
}

# Replace literal "NaN" strings with NA in character columns (exclude certain cols)
.wa_naify_nan_chars <- function(df, exclude = character()) {
  cols <- setdiff(names(df), exclude)
  for (nm in cols) {
    if (is.character(df[[nm]])) {
      x <- df[[nm]]
      x[x == "NaN"] <- NA
      df[[nm]] <- x
    }
  }
  df
}

# --------------------------------------------------------------------------

#' Import a GAMET output file into R.
#'
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr mutate
#' @importFrom tidyselect where
#' @param path A string giving the path and filename to import.
#' @export
#' @seealso \code{\link{predict_quality}}
import_gamet <- function(path) {
  dat1 <- utils::read.csv(path, header = TRUE, stringsAsFactors = FALSE)

  # normalize ID
  dat1$filename <- basename(tools::file_path_sans_ext(dat1$filename))
  names(dat1)[names(dat1) == "filename"] <- "ID"
  dat1$ID <- as.character(dat1$ID)

  # clean character "NaN" -> NA; auto-convert numeric-like chars (keep ID as character)
  dat1 <- .wa_naify_nan_chars(dat1, exclude = "ID")
  dat1 <- .wa_convert_numeric_like(dat1, exclude = "ID")

  # sort by ID (character-safe)
  dat1 <- dat1[order(dat1$ID), ]

  # select and derive
  dat4 <- dat1[, c("ID", "error_count", "word_count", "grammar", "misspelling",
                   "duplication", "typographical", "whitespace")]

  # guard against division by zero
  dat4$per_gram  <- ifelse(dat4$word_count == 0, NA_real_, dat4$grammar    / dat4$word_count)
  dat4$per_spell <- ifelse(dat4$word_count == 0, NA_real_, dat4$misspelling / dat4$word_count)

  dat4
}

#' Import a Coh-Metrix output file (.csv) into R.
#'
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr mutate
#' @importFrom tidyselect where
#' @param path A string giving the path and filename to import.
#' @export
#' @seealso \code{\link{predict_quality}}
import_coh <- function(path) {
  dat1 <- utils::read.csv(path, header = TRUE, stringsAsFactors = FALSE)

  # normalize ID
  dat1$TextID <- basename(tools::file_path_sans_ext(dat1$TextID))
  names(dat1)[names(dat1) == "TextID"] <- "ID"
  dat1$ID <- as.character(dat1$ID)

  # clean character "NaN" -> NA; auto-convert numeric-like chars (keep ID as character)
  dat1 <- .wa_naify_nan_chars(dat1, exclude = "ID")
  dat1 <- .wa_convert_numeric_like(dat1, exclude = "ID")

  # sort by ID
  dat1 <- dat1[order(dat1$ID), ]
  dat1
}

#' Import a ReaderBench output file (.csv) into R.
#'
#' @importFrom magrittr %>%
#' @importFrom utils read.table
#' @importFrom dplyr mutate
#' @importFrom tidyselect where
#' @param path A string giving the path and filename to import.
#' @export
#' @seealso \code{\link{predict_quality}}
import_rb <- function(path) {
  # check first line for "SEP=,"; if present, skip that line on import
  con <- file(path, "r")
  first_line <- readLines(con, n = 1)
  close(con)

  if (identical(first_line, "SEP=,")) {
    dat_RB <- utils::read.table(
      text = readLines(path, warn = FALSE),
      header = TRUE, sep = ",", skip = 1, stringsAsFactors = FALSE, check.names = TRUE
    )
  } else {
    dat_RB <- utils::read.table(
      text = readLines(path, warn = FALSE),
      header = TRUE, sep = ",", stringsAsFactors = FALSE, check.names = TRUE
    )
  }

  # replace "NaN" (as strings) in character cols only
  dat_RB <- .wa_naify_nan_chars(dat_RB)

  # drop sentiment columns (keep first 404) and normalize ID
  dat_RB2 <- dat_RB[, 1:404]
  names(dat_RB2)[names(dat_RB2) == "File.name"] <- "ID"
  dat_RB2$ID <- as.character(dat_RB2$ID)

  # auto-convert numeric-like character columns to numeric, excluding ID
  dat_RB2 <- .wa_convert_numeric_like(dat_RB2, exclude = "ID")

  # sort by ID
  dat_RB2 <- dat_RB2[order(dat_RB2$ID), ]
  dat_RB2
}

#' Import a ReaderBench output file (.csv) and GAMET output file (.csv),
#' and merge the two files on ID.
#'
#' @importFrom magrittr %>%
#' @importFrom utils read.csv read.table
#' @importFrom tools file_path_sans_ext
#' @export
#' @seealso \code{\link{predict_quality}}
#' @param rb_path A string giving the path and ReaderBench filename to import.
#' @param gamet_path A string giving the path and GAMET filename to import.
import_merge_gamet_rb <- function(rb_path, gamet_path) {
  dat.RB <- import_rb(rb_path)
  dat.G  <- import_gamet(gamet_path)
  # both imports keep ID as character; merge on "ID"
  dat.merge <- merge(dat.G, dat.RB, by = "ID", all = FALSE)
  dat.merge
}

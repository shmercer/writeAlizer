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

# Internal: derive keep/exclude RB feature names from packaged sample file
# We read ONLY the header (nrows=0). If the file has a "SEP=," first line,
# we skip it. Names are made syntactic (check.names=TRUE), so "File name" -> "File.name".
.wa_rb_keep_exclude_from_sample <- function() {
  path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  if (!nzchar(path) || !file.exists(path)) return(NULL)

  first <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
  skip <- if (identical(first, "SEP=,")) 1L else 0L

  # read header only, no data
  hdr <- utils::read.csv(path, nrows = 0, check.names = TRUE, skip = skip)
  nm  <- colnames(hdr)
  if (!length(nm)) return(NULL)

  keep <- nm[seq_len(min(404L, length(nm)))]
  drop <- if (length(nm) > 404L) nm[(404L + 1L):length(nm)] else character(0)

  list(keep = keep, drop = drop)
}

# Internal: validate required columns and IDs
.wa_validate_import <- function(df, required, context = "import") {
  # Try to coerce to a data frame if possible (tibble, list of equal-length vectors, etc.)
  if (!is.data.frame(df)) {
    df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE),
                   error = function(e) df)
  }
  if (!is.data.frame(df)) {
    rlang::abort(
      paste0("Internal error: ", context, " did not return a data.frame."),
      .subclass = "writeAlizer_input_error"
    )
  }

  # Required columns present?
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    rlang::abort(
      paste0("Missing required column(s) for ", context, ": ",
             paste(missing, collapse = ", ")),
      .subclass = "writeAlizer_input_error"
    )
  }

  # ID column must exist, be character, and unique
  if (!"ID" %in% names(df)) {
    rlang::abort("Imported data must contain an 'ID' column.", .subclass = "writeAlizer_input_error")
  }
  df[["ID"]] <- as.character(df[["ID"]])

  dups <- df$ID[duplicated(df$ID)]
  if (length(dups)) {
    rlang::abort(
      paste0("Duplicate IDs detected (", length(unique(dups)), "): e.g., ",
             paste(utils::head(unique(dups), 3L), collapse = ", ")),
      .subclass = "writeAlizer_input_error"
    )
  }

  df
}

# --------------------------------------------------------------------------

#' @title Extract the filename stem before ".txt"
#' @description
#' Removes any directory path and optional `.txt` extension from
#' filenames or file paths. This function standardizes text identifiers
#' across Coh-Metrix, GAMET, and other text analysis outputs that may
#' include full paths or extensions in their ID fields.
#'
#' @param x A character vector (or coercible) containing file paths or
#' filenames. Elements may or may not include a `.txt` suffix or any
#' directory path.
#'
#' @return A character vector where each element is reduced to the
#' final path component, with any trailing `.txt` (case-insensitive)
#' removed. `NA` values are preserved as `NA_character_`.
#'
#' @details
#' The function handles both forward (`/`) and backward (`\\`) slashes
#' in file paths. If a value has no path and/or no `.txt` suffix, it is
#' returned unchanged (aside from coercion to character).
#'
#' @examples
#' keep_stem_before_txt(c(
#'   "C:/data/3401.txt",
#'   "E:\\\\samples\\\\1002.TXT",
#'   "plain_id",
#'   NA
#' ))
#' #> [1] "3401" "1002" "plain_id" NA
#'
#' @export
keep_stem_before_txt <- function(x) {
  original_na <- is.na(x)
  out <- as.character(x)
  not_na <- !original_na
  # Strip directory path, then remove .txt (case-insensitive)
  out[not_na] <- sub("\\.txt$", "", sub("^.*[\\\\/]", "", out[not_na]), ignore.case = TRUE)
  out[original_na] <- NA_character_
  out
}

#' Import a GAMET output file into R.
#'
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom dplyr mutate
#' @importFrom tidyselect where
#' @param path A string giving the path and filename to import.
#' @export
#' @seealso \code{\link{predict_quality}}
#' @return
#' A base \code{data.frame} with one row per record and the following columns:
#' \itemize{
#'   \item \code{ID} (\code{character}): unique identifier of the text/essay.
#'   \item One column per retained GAMET error/category variable (\code{numeric};
#'         typically counts or rates). Column names follow the GAMET output
#'         variable names.
#' }
#' The object has class \code{data.frame} (or \code{tibble} if converted by the user).
#' @examples
#' # Example with package sample data
#' file_path   <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
#' gamet_file  <- import_gamet(file_path)
#' head(gamet_file)
import_gamet <- function(path) {
  dat1 <- utils::read.csv(path, header = TRUE, stringsAsFactors = FALSE)

  # normalize ID from the 'filename' column (works with or without path/.txt)
  dat1$filename <- keep_stem_before_txt(dat1$filename)
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
  dat4$per_gram  <- ifelse(dat4$word_count == 0, NA_real_, dat4$grammar     / dat4$word_count)
  dat4$per_spell <- ifelse(dat4$word_count == 0, NA_real_, dat4$misspelling / dat4$word_count)

  # validate IDs
  dat4 <- .wa_validate_import(dat4, required = c("ID"), context = "import_gamet")

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
#' @return
#' A base \code{data.frame} with one row per record and the following columns:
#' \itemize{
#'   \item \code{ID} (\code{character}): unique identifier of the text/essay.
#'   \item One column per retained Coh\emph{-}Metrix feature, kept by original
#'         feature name (\code{numeric}). Feature names mirror the Coh\emph{-}Metrix
#'         output variables.
#' }
#' The object has class \code{data.frame} (or \code{tibble} if converted by the user).
#' @examples
#' # Example with package sample data
#' file_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
#' coh_file  <- import_coh(file_path)
#' head(coh_file)
import_coh <- function(path) {
  dat1 <- utils::read.csv(path, header = TRUE, stringsAsFactors = FALSE)

  # normalize ID from the 'TextID' column (works with or without path/.txt)
  dat1$TextID <- keep_stem_before_txt(dat1$TextID)
  names(dat1)[names(dat1) == "TextID"] <- "ID"
  dat1$ID <- as.character(dat1$ID)

  # clean character "NaN" -> NA; auto-convert numeric-like chars (keep ID as character)
  dat1 <- .wa_naify_nan_chars(dat1, exclude = "ID")
  dat1 <- .wa_convert_numeric_like(dat1, exclude = "ID")

  # sort by ID
  dat1 <- dat1[order(dat1$ID), ]

  # validate IDs
  dat1 <- .wa_validate_import(dat1, required = c("ID"), context = "import_coh")

  dat1
}

#' Import a ReaderBench output file (.csv) into R.
#'
#' When available, the function reads the header of the packaged sample
#' (\code{inst/extdata/sample_rb.csv}) and keeps the first 404 columns by NAME
#' (plus the \code{File.name}/\code{ID} column), excluding any columns with names
#' appearing after position 404 in that header. If the sample is unavailable,
#' it falls back to keeping the first 404 columns by position.
#' @importFrom magrittr %>%
#' @importFrom utils read.table
#' @importFrom dplyr mutate
#' @importFrom tidyselect where
#' @param path A string giving the path and filename to import.
#' @export
#' @seealso \code{\link{predict_quality}}
#' @return
#' A base \code{data.frame} with one row per record and the following columns:
#' \itemize{
#'   \item \code{ID} (\code{character}): unique identifier of the text/essay.
#'   \item One column per retained ReaderBench feature, kept by original
#'         feature name (\code{numeric}). Feature names mirror the ReaderBench
#'         output variables.
#' }
#' The object has class \code{data.frame} (or \code{tibble} if converted by the user).
#' @examples
#' # Fast, runnable example with package sample data
#' file_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
#' rb_file   <- import_rb(file_path)
#' head(rb_file)
import_rb <- function(path) {
  # --- robust header detection (handles empty file edge cases) ---
  con <- file(path, "r")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  first_line <- tryCatch(readLines(con, n = 1L), error = function(e) character(0))
  first_line <- if (length(first_line)) first_line else ""

  # Read CSV (ReaderBench sometimes writes "SEP=," in the first line)
  txt <- readLines(path, warn = FALSE)
  if (identical(first_line, "SEP=,")) {
    dat_RB <- utils::read.table(
      text = txt, header = TRUE, sep = ",", skip = 1L,
      stringsAsFactors = FALSE, check.names = TRUE
    )
  } else {
    dat_RB <- utils::read.table(
      text = txt, header = TRUE, sep = ",",
      stringsAsFactors = FALSE, check.names = TRUE
    )
  }

  # Replace "NaN" strings in character columns only
  dat_RB <- .wa_naify_nan_chars(dat_RB)

  # Name-based selection using the packaged sample header, if available:
  # - keep: first 404 NAMES from sample_rb.csv (syntactic)
  # - drop: names beyond 404, if present
  # Fall back: first 404 columns by POSITION.
  k <- .wa_rb_keep_exclude_from_sample()

  if (!is.null(k)) {
    # Always ensure ID/File.name are retained if present
    base_keep <- c("File.name", "ID")
    keep_names <- unique(c(base_keep, setdiff(k$keep, base_keep)))
    keep_names <- intersect(colnames(dat_RB), keep_names)
    dat_RB2 <- dat_RB[, keep_names, drop = FALSE]

    # Drop any explicitly excluded names if they slipped in
    if (length(k$drop)) {
      drop_these <- intersect(colnames(dat_RB2), k$drop)
      if (length(drop_these)) {
        dat_RB2 <- dat_RB2[, setdiff(colnames(dat_RB2), drop_these), drop = FALSE]
      }
    }
  } else {
    # Fallback: positional subset (keep first 404 cols if they exist)
    n <- min(404L, ncol(dat_RB))
    dat_RB2 <- dat_RB[, seq_len(n), drop = FALSE]
  }

  # --- Normalize ID before any validation ---
  # Priority:
  #  1) If File.name present in the current subset, rename to ID
  #  2) Else if ID already present (some exports), keep as-is
  #  3) Else if File.name existed in the original, construct ID from it
  if ("File.name" %in% names(dat_RB2)) {
    names(dat_RB2)[names(dat_RB2) == "File.name"] <- "ID"
  } else if (!"ID" %in% names(dat_RB2) && "File.name" %in% names(dat_RB)) {
    dat_RB2$ID <- as.character(dat_RB[["File.name"]])
  }

  # Ensure character type for ID if present now
  if ("ID" %in% names(dat_RB2)) {
    dat_RB2$ID <- as.character(dat_RB2$ID)
  }

  # Auto-convert numeric-like character columns to numeric, excluding ID
  dat_RB2 <- .wa_convert_numeric_like(dat_RB2, exclude = "ID")

  # Stable sort by ID when available (no-op if ID missing; validator will catch)
  if ("ID" %in% names(dat_RB2)) {
    dat_RB2 <- dat_RB2[order(dat_RB2$ID), , drop = FALSE]
  }

  # --- Validate IDs (required + uniqueness) ---
  dat_RB2 <- .wa_validate_import(dat_RB2, required = c("ID"), context = "import_rb")

  # Ensure a base data.frame (not tibble) for downstream expectations
  dat_RB2 <- as.data.frame(dat_RB2, stringsAsFactors = FALSE)
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
#' @return
#' A base \code{data.frame} created by joining the ReaderBench and GAMET tables
#' by \code{ID}, with one row per matched ID and the following columns:
#' \itemize{
#'   \item \code{ID} (\code{character}): identifier present in both sources.
#'   \item All retained ReaderBench feature columns (\code{numeric}).
#'   \item All retained GAMET error/category columns (\code{numeric}).
#' }
#' By default, only IDs present in both inputs are kept (inner join). If a
#' feature name appears in both sources, standard merge suffixes (e.g.,
#' \code{.x}/\code{.y}) may be applied by the join implementation.
#' The object has class \code{data.frame} (or \code{tibble} if converted by the user).
#' @examples
#' # Example with package sample data
#' rb_path   <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
#' gam_path  <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
#' rb_gam    <- import_merge_gamet_rb(rb_path, gam_path)
#' head(rb_gam)
import_merge_gamet_rb <- function(rb_path, gamet_path) {
  dat.RB <- import_rb(rb_path)
  dat.G  <- import_gamet(gamet_path)
  # both imports keep ID as character; merge on "ID"
  dat.merge <- merge(dat.G, dat.RB, by = "ID", all = FALSE)
  dat.merge
}

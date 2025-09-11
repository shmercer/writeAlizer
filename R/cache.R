# R/cache.R

# User cache directory for writeAlizer artifacts (RDS/RDA files)
.wa_cache_dir <- function() {
  dir <- tools::R_user_dir("writeAlizer", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

# Build a path inside the cache
.wa_cached_path <- function(fname) file.path(.wa_cache_dir(), fname)

# Ensure a file is present in the cache; download if missing; verify checksum if provided.
# In tests, you can set options(writeAlizer.mock_dir = "<path>") to provide prebuilt artifacts.
.wa_ensure_file <- function(filename, url, sha256 = NULL, quiet = FALSE) {
  # Test hook: use a mock directory (skip checksum)
  mock_dir <- getOption("writeAlizer.mock_dir", default = NULL)
  if (!is.null(mock_dir) && nzchar(mock_dir)) {
    dest <- file.path(mock_dir, filename)
    if (!file.exists(dest)) stop("Mock artifact not found: ", dest, call. = FALSE)
    return(dest)
  }

  dest <- .wa_cached_path(filename)
  need_download <- !file.exists(dest)

  # If we have the file and a checksum, verify it
  if (!need_download && !is.null(sha256) && nzchar(sha256)) {
    got <- tryCatch(digest::digest(dest, algo = "sha256", file = TRUE),
                    error = function(e) NA_character_)
    if (!identical(got, sha256)) {
      warning(sprintf("Checksum mismatch for cached %s; re-downloading", filename))
      need_download <- TRUE
    }
  }

  if (need_download) {
    utils::download.file(url = url, destfile = dest, mode = "wb", quiet = quiet)
    if (!file.exists(dest)) stop("Download failed for ", filename, call. = FALSE)
  }

  # Verify after (re)download
  if (!is.null(sha256) && nzchar(sha256)) {
    got <- tryCatch(digest::digest(dest, algo = "sha256", file = TRUE),
                    error = function(e) NA_character_)
    if (!identical(got, sha256)) {
      stop(sprintf("Checksum mismatch for %s (expected %s, got %s)",
                   filename, sha256, got), call. = FALSE)
    }
  }

  dest
}

# Optional: a tiny wrapper you can call elsewhere (kept for compatibility)
#' @title Download artifact into user cache (with optional checksum)
#' @description Download a registry artifact (RDS/RDA/etc.) into the writeAlizer
#' user cache (via \code{tools::R_user_dir}) and optionally verify its SHA-256 checksum.
#' This is a thin wrapper over the internal cache helper used by the package.
#' @param file Character scalar. Filename to write within the cache (e.g., \code{"rb_mod2a.rda"}).
#' @param url Character scalar. Remote URL to download from.
#' @param sha256 Optional character scalar. Expected SHA-256 hex digest of the file
#'   contents. If provided, the downloaded file is verified; a mismatch raises an error.
#' @param quiet Logical. Passed to \code{utils::download.file}; \code{TRUE} suppresses progress output.
#' @return The full filesystem path (character scalar) to the cached file.
#' @seealso \code{\link{preprocess}}, \code{\link{predict_quality}}
#' @export
wa_download <- function(file, url, sha256 = NULL, quiet = FALSE) {
  .wa_ensure_file(file, url, sha256 = sha256, quiet = quiet)
}

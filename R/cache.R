# R/cache.R

# User cache directory for writeAlizer artifacts (RDS/RDA files)
.wa_cache_dir <- function() {
  dir <- tools::R_user_dir("writeAlizer", which = "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
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
wa_download <- function(file, url, sha256 = NULL, quiet = FALSE) {
  .wa_ensure_file(file, url, sha256 = sha256, quiet = quiet)
}

#' Download artifact into cache with optional checksum
#'
#' Internal helper used by writeAlizer to fetch an artifact into the cache.
#' Returns the absolute path to the cached file.
#'
#' @param file Character scalar; filename to use in the cache (e.g., `"rb_mod1a.rda"`).
#' @param url  Character scalar; source URL. May be a `file://` URL for local testing.
#' @param sha256 Optional 64-hex SHA-256 checksum for verification. If provided,
#'   the downloaded/cached file must match it (or re-download is attempted).
#' @param quiet Logical; if `TRUE`, suppresses download progress messages.
#'
#' @return A character scalar: the absolute path to the cached file.
#' @importFrom utils download.file
#' @importFrom digest digest
#' @export
#' @examples
#' # Offline-friendly example using a local source (no network):
#' src <- tempfile(fileext = ".bin")
#' writeBin(as.raw(1:10), src)
#' dest <- wa_download("example.bin", url = paste0("file:///", normalizePath(src, winslash = "/")))
#' file.exists(dest)
wa_download <- function(file, url, sha256 = NULL, quiet = TRUE) {
  dest <- .wa_cached_path(file)
  utils::download.file(url = url, destfile = dest, mode = "wb", quiet = quiet)
  if (!is.null(sha256)) {
    got <- tryCatch(digest::digest(dest, algo = "sha256", file = TRUE),
                    error = function(e) NA_character_)
    if (!identical(got, sha256)) {
      stop(sprintf("Checksum mismatch for %s (expected %s, got %s)", file, sha256, got),
           call. = FALSE)
    }
  }
  dest
}

#' @export
#' @rdname wa_download
#' @usage download(file, url) # deprecated
download <- function(file, url) {
  .Deprecated("wa_download")
  wa_download(file, url)
}

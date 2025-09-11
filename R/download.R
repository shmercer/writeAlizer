# R/download.R
#' @title Download artifact into cache with optional checksum
#' @export
#' @importFrom utils download.file
#' @importFrom digest digest
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

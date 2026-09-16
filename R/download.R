#' Download and cache an artifact (graceful offline behavior)
#'
#' Public helper to fetch an artifact into the user cache. This function
#' delegates to the internal downloader used by the package at runtime,
#' so it benefits from the same behavior:
#'
#' - Respects \code{options(writeAlizer.mock_dir)} to load local mock copies
#'   (useful for tests/examples and offline runs).
#' - Fails \emph{gracefully} with a clear, informative message when Internet
#'   resources are unavailable or have changed (per CRAN policy).
#' - Verifies an optional SHA-256 checksum and re-downloads or errors if it
#'   does not match.
#'
#' @param file Character scalar; relative filename to use in the cache (e.g., `"rb_mod1a.rda"`).
#' @param url  Character scalar; source URL. May be a `file://` URL for local testing.
#' @param sha256 Optional 64-hex SHA-256 checksum for verification. If provided,
#'   the cached file must match it (or a re-download is attempted). NULL, NA,
#'   and an empty string mean no checksum. Local mock artifacts bypass checksums.
#' @param quiet Logical; if `TRUE`, suppresses download progress messages.
#'
#' @details
#' Local file URLs work offline. Internet URLs require a connection unless a
#' valid cached copy exists. Downloads are verified before replacing a cached
#' file, and failed transfers leave the previous copy intact. Nested relative
#' filenames are accepted; absolute names and parent-directory traversal are not.
#' The filename is the cache key: changing only the URL does not refresh an
#' existing file. Supply the expected checksum or clear the cache to refresh it.
#' @return A character scalar: the absolute path to the cached file, or the local
#'   artifact path when \code{writeAlizer.mock_dir} is set.
#' @export
#'
#' @examples
#' local({
#'   cache <- tempfile("wa-cache-")
#'   old <- options(writeAlizer.cache_dir = cache,
#'                  writeAlizer.mock_dir = NULL, writeAlizer.offline = TRUE)
#'   on.exit(options(old))
#'   on.exit(unlink(cache, recursive = TRUE), add = TRUE)
#'   src <- tempfile(fileext = ".bin")
#'   writeBin(as.raw(1:10), src)
#'   on.exit(unlink(src), add = TRUE)
#'   path <- normalizePath(src, winslash = "/")
#'   url <- paste0(if (.Platform$OS.type == "windows") "file:///" else "file://",
#'                 utils::URLencode(path))
#'   sha <- digest::digest(src, algo = "sha256", file = TRUE)
#'   dest <- wa_download("example.bin", url, sha256 = sha)
#'   file.exists(dest)
#' })
wa_download <- function(file, url, sha256 = NULL, quiet = TRUE) {
  .wa_ensure_file(file = file, url = url, sha256 = sha256, quiet = quiet)
}

#' @export
#' @rdname wa_download
#' @usage download(file, url) # deprecated
download <- function(file, url) {
  .Deprecated("wa_download")
  wa_download(file, url)
}

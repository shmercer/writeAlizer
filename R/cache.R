#' Path to writeAlizer's user cache
#'
#' Returns the directory used to store cached model artifacts. By default this is
#' a platform-appropriate user cache path from \code{tools::R_user_dir("writeAlizer","cache")}.
#' If the option \code{writeAlizer.cache_dir} is set to a non-empty string, that
#' location is used instead. This makes it easy to redirect the cache during tests
#' or examples (e.g., to \code{tempdir()}).
#'
#' @return Character scalar path.
#' @seealso \code{\link{wa_cache_clear}}
#' @examples
#' # Inspect the cache directory (no side effects)
#' wa_cache_dir()
#'
#' # Safe demo: redirect cache to a temp folder, create a file, then clear it
#' \dontshow{
#' old <- getOption("writeAlizer.cache_dir"); on.exit(options(writeAlizer.cache_dir = old), add = TRUE)
#' tmp <- file.path(tempdir(), "wa_cache_demo"); dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
#' options(writeAlizer.cache_dir = tmp)
#' writeLines("demo", file.path(wa_cache_dir(), "demo.txt"))
#' wa_cache_clear(ask = FALSE)
#' }
#' @export
wa_cache_dir <- function() {
  override <- getOption("writeAlizer.cache_dir", NULL)
  if (is.character(override) && nzchar(override)) return(override)
  tools::R_user_dir("writeAlizer", "cache")
}

# Internal: build a full cache file path for a given filename.
# Used by the artifact loader when present.
#' @keywords internal
#' @noRd
.wa_cached_path <- function(filename) file.path(wa_cache_dir(), filename)

#' Clear writeAlizer's user cache
#'
#' Deletes all files under \code{wa_cache_dir()}. If \code{ask = TRUE} \emph{and} in an
#' interactive session, a short preview (item count, total size, and up to 10 sample
#' paths) is printed before asking for confirmation.
#'
#' @param ask Logical; if \code{TRUE} and interactive, ask for confirmation.
#' @param preview Logical; if \code{TRUE} and \code{ask} is \code{TRUE}, show a brief
#'   listing/size summary before asking.
#' @return Invisibly returns \code{TRUE} if the cache was cleared (or already absent),
#'   \code{FALSE} if the user declined or deletion failed.
#' @seealso \code{\link{wa_cache_dir}}
#' @examples
#' # Safe demo: redirect cache to tempdir(), create a file, then clear it
#' \dontshow{
#' old <- getOption("writeAlizer.cache_dir"); on.exit(options(writeAlizer.cache_dir = old), add = TRUE)
#' tmp <- file.path(tempdir(), "wa_cache_demo2"); dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
#' options(writeAlizer.cache_dir = tmp)
#' writeLines("demo", file.path(wa_cache_dir(), "demo.txt"))
#' wa_cache_clear(ask = FALSE)
#' }
#' @export
wa_cache_clear <- function(ask = interactive(), preview = TRUE) {
  path <- wa_cache_dir()
  if (!dir.exists(path)) {
    message("Cache directory does not exist: ", path)
    return(invisible(TRUE))
  }

  # Preview
  if (isTRUE(ask) && interactive() && isTRUE(preview)) {
    files <- list.files(path, all.files = TRUE, full.names = TRUE,
                        recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
    n <- length(files)
    sizes <- tryCatch(sum(file.info(files)$size, na.rm = TRUE), error = function(e) NA_real_)
    fmt_size <- function(b) {
      if (is.na(b)) return("unknown")
      units <- c("B","KB","MB","GB","TB")
      if (b <= 0) return("0 B")
      i <- floor(log(b, 1024)); i <- max(0, min(i, length(units)-1))
      sprintf("%.1f %s", b / (1024^i), units[i+1])
    }
    message("About to delete ", n, " item", if (n != 1) "s" else "", " (", fmt_size(sizes), ") under:\n  ", path)
    if (n) {
      preview_n <- min(10L, n)
      message("Preview (first ", preview_n, "):\n  ", paste(files[seq_len(preview_n)], collapse = "\n  "))
    }
  }

  proceed <- TRUE
  if (isTRUE(ask) && interactive()) {
    ans <- utils::menu(c("No", "Yes"),
                       title = paste0("Delete ALL files under\n  ", path, "\n?"))
    proceed <- identical(ans, 2L)
  }
  if (!proceed) return(invisible(FALSE))

  ok <- unlink(path, recursive = TRUE, force = TRUE) == 0
  if (ok) {
    message("Cleared cache: ", path)
    # Re-create empty directory so downstream code expecting it won't fail
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  } else {
    warning("Failed to clear: ", path)
  }
  invisible(ok)
}

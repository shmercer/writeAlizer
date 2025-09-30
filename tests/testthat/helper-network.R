# Helper to extract all artifact URLs for one or more model keys
# Uses the internal registry accessor to mirror production behavior.
.wa_urls_for_models <- function(keys) {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")
  keys <- unique(as.character(keys))

  # Query both RDA and RDS parts to cover fits + varlists
  all <- lapply(keys, function(k) {
    rds <- tryCatch(parts_for("rds", k), error = function(e) NULL)
    rda <- tryCatch(parts_for("rda", k), error = function(e) NULL)
    do.call(rbind, Filter(Negate(is.null), list(rds, rda)))
  })
  urls <- unique(as.character(do.call(rbind, all)$url))
  urls[is.finite(match(urls, urls))]  # drop NAs / keep unique
}

# Skip the calling test if *any* of the given URLs/paths are unavailable.
# - file:// URLs: require the underlying file to exist.
# - http(s) URLs: require internet; optionally attempt a very fast HEAD.
skip_if_unavailable <- function(urls, reason = "required artifact(s) unavailable") {
  urls <- as.character(urls)
  urls <- urls[nzchar(urls)]
  if (!length(urls)) return(invisible(TRUE))

  # Allow tests to force offline behavior:
  if (isTRUE(getOption("writeAlizer.offline", FALSE))) {
    testthat::skip(paste(reason, "(offline mode forced via option)"))
  }

  has_curl <- requireNamespace("curl", quietly = TRUE)
  has_net  <- if (has_curl) curl::has_internet() else TRUE  # best-effort

  # Quick checks first
  for (u in urls) {
    lu <- tolower(u)
    if (startsWith(lu, "file://")) {
      # Work across OSes; strip scheme then normalize
      path <- sub("^file:/+", "/", u)
      if (!file.exists(path)) {
        testthat::skip(paste0(reason, ": missing local file\n  ", path))
      }
    } else if (startsWith(lu, "http://") || startsWith(lu, "https://")) {
      if (!has_net) {
        testthat::skip(paste0(reason, ": no internet"))
      }
    }
  }

  # Optional: fast HEAD probe for http(s) if curl is available
  if (has_curl) {
    h <- curl::new_handle(nobody = TRUE, followlocation = TRUE)
    for (u in urls) {
      if (grepl("^https?://", u, ignore.case = TRUE)) {
        ok <- tryCatch({
          # 2s connect, 3s total; CRAN-friendly and non-flaky
          curl::handle_setopt(h, connecttimeout = 2, timeout = 3)
          curl::curl_fetch_memory(u, handle = h)
          TRUE
        }, error = function(e) FALSE)
        if (!ok) {
          testthat::skip(paste0(reason, ": HEAD failed\n  ", u))
        }
      }
    }
  }

  invisible(TRUE)
}

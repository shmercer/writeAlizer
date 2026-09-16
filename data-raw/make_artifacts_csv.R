# Refresh checksums for the URLs in the shipped registry.
# Run explicitly from the repository root; this downloads every unique artifact
# and replaces inst/metadata/artifacts.csv. It does not retrain or add models.
local({
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install pkgload to run this maintenance script.")
  }
  pkgload::load_all(".")
  cache <- tempfile("writeAlizer-registry-")
  old <- options(timeout = max(600, getOption("timeout", 60)),
                 writeAlizer.cache_dir = cache, writeAlizer.mock_dir = NULL,
                 writeAlizer.offline = FALSE, writeAlizer.registry_csv = NULL)
  on.exit(options(old))
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  arts <- getFromNamespace(".wa_registry", "writeAlizer")()
  for (i in seq_len(nrow(arts))) {
    message(sprintf("[%d/%d] %s", i, nrow(arts), arts$file[i]))
    # Fresh temporary cache: never establish new checksums from a user's old cache.
    dest <- writeAlizer::wa_download(arts$file[i], arts$url[i], sha256 = NULL)
    arts$sha[i] <- digest::digest(dest, algo = "sha256", file = TRUE)
  }
  out <- file.path("inst", "metadata", "artifacts.csv")
  utils::write.csv(arts, out, row.names = FALSE)
  message("Wrote: ", out)
})

# data-raw/make_artifacts_csv.R
# Compute SHA-256 checksums for all artifacts in .wa_artifacts()
# and write inst/metadata/artifacts.csv

# Increase timeout for large downloads
options(timeout = max(600, getOption("timeout", 60)))

# Load your package code so .wa_artifacts() is available
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
}

# ---------- Resolve artifacts ----------
arts <- NULL
if (exists(".wa_artifacts", mode = "function")) {
  arts <- .wa_artifacts()
}
if (is.null(arts) || !is.data.frame(arts)) {
  stop("Could not find .wa_artifacts(). Run this script from the package repo after devtools::load_all('.').")
}

# Keep just the columns we need and drop dupes
keep_cols <- intersect(c("kind", "model", "part", "file", "url"), names(arts))
arts <- unique(arts[keep_cols])

# ---------- Where to store the downloaded files while hashing ----------
# Prefer package cache if available; else use tempdir()
ensure_path <- function(fname, url, quiet = FALSE) {
  # If you already defined .wa_cached_path/.wa_ensure_file in R/cache.R, use them:
  if (exists(".wa_ensure_file", mode = "function")) {
    # Try to lookup sha if it already exists in a CSV (optional)
    sha <- NA_character_
    return(.wa_ensure_file(fname, url, sha256 = sha, quiet = quiet))
  }
  # Fallback: data-raw/tmp cache
  dir.create("data-raw/.cache", recursive = TRUE, showWarnings = FALSE)
  dest <- file.path("data-raw/.cache", fname)
  if (!file.exists(dest)) {
    utils::download.file(url, destfile = dest, mode = "wb", quiet = quiet)
  }
  dest
}

# ---------- Compute SHA-256 ----------
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Please install 'digest' first: install.packages('digest')")
}

sha_vec <- character(nrow(arts))
for (i in seq_len(nrow(arts))) {
  f <- arts$file[i]
  u <- arts$url[i]
  message(sprintf("[%3d/%3d] %s", i, nrow(arts), f))
  dest <- ensure_path(f, u, quiet = FALSE)
  sha_vec[i] <- digest::digest(dest, algo = "sha256", file = TRUE)
}

arts$sha <- sha_vec

# ---------- Write CSV into the package ----------
out <- file.path("inst", "metadata", "artifacts.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(arts, out, row.names = FALSE)
message("Wrote: ", out)

# Recommended: ignore data-raw in builds
if (file.exists(".Rbuildignore")) {
  # Append ^data-raw$ if not present
  rb <- readLines(".Rbuildignore", warn = FALSE)
  if (!any(grepl("^\\^data-raw\\$", rb))) {
    writeLines(c(rb, "^data-raw$"), ".Rbuildignore")
    message("Added ^data-raw$ to .Rbuildignore")
  }
} else {
  writeLines("^data-raw$", ".Rbuildignore")
  message("Created .Rbuildignore and added ^data-raw$")
}

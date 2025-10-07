# ------- helpers (internal; do NOT export) -------

.wa_canonical_model <- function(model) {
  switch(model,
         "rb_mod3narr" = "rb_mod3narr_v2",
         "rb_mod3exp"  = "rb_mod3exp_v2",
         "rb_mod3per"  = "rb_mod3per_v2",
         "rb_mod3all"  = "rb_mod3all_v2",
         "example"     = "example",
         model
  )
}

# Read the shipped CSV registry (required).
.wa_registry <- function() {
  # Allow tests (or power users) to override the registry CSV location.
  csv_opt <- getOption("writeAlizer.registry_csv", NULL)
  if (is.character(csv_opt) && nzchar(csv_opt)) {
    csv <- csv_opt
  } else {
    csv <- system.file("metadata", "artifacts.csv", package = "writeAlizer")
  }

  if (!nzchar(csv) || !file.exists(csv)) {
    rlang::abort(
      c(
        "Registry CSV not found (inst/metadata/artifacts.csv)." =
          "Reinstall writeAlizer or ensure the file is included in the package.",
        "You can also set options(writeAlizer.registry_csv = '/path/to/artifacts.csv') to override during tests."
      ),
      .subclass = "writeAlizer_registry_missing"
    )
  }

  df <- utils::read.csv(csv, stringsAsFactors = FALSE)

  need <- c("kind","model","part","file","url","sha")
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    rlang::abort(
      paste0("artifacts.csv missing columns: ", paste(miss, collapse = ", ")),
      .subclass = "writeAlizer_registry_malformed"
    )
  }
  df
}

# Helper to filter registry by kind/model and return rows (including sha)
.wa_parts_for <- function(kind, model) {
  reg <- .wa_registry()
  if (!all(c("kind","model","part","file","url") %in% names(reg))) {
    rlang::abort("artifacts registry is missing required columns.",
                 .subclass = "writeAlizer_registry_malformed")
  }
  if (!is.character(kind) || length(kind) != 1L || !nzchar(kind)) {
    rlang::abort("`kind` must be a single string ('rds' or 'rda').",
                 .subclass = "writeAlizer_input_error")
  }
  key <- if (exists(".wa_canonical_model", mode = "function")) .wa_canonical_model(model) else model
  out <- reg[reg$kind == kind & reg$model == key, , drop = FALSE]
  if ("part" %in% names(out)) {
    out <- out[order(out$part), , drop = FALSE]
  }
  out
}

.wa_local_path <- function(filename) {
  file.path(system.file("extdata", package = "writeAlizer"), filename)
}

# Internal: convert file:// URL to local path, cross-platform
# Normalize a file:// URL into a local filesystem path
# - POSIX: keep leading "/" → "/private/var/..."
# - Windows: drop leading "/" before drive letter → "C:/...", then backslashes
.wa_from_file_url <- function(url) {
  stopifnot(is.character(url), length(url) == 1L, nzchar(url))
  if (!startsWith(url, "file://")) return(url)

  # Ensure exactly one leading slash after the scheme for local paths
  # e.g., "file:///tmp/x" -> "/tmp/x"
  #       "file://C:/x"   -> "/C:/x" (we handle Windows below)
  p <- sub("^file://+", "/", url, perl = TRUE)

  # URL-decode (%20 etc.)
  p <- utils::URLdecode(p)

  if (.Platform$OS.type == "windows") {
    # UNC host form: file://server/share/path -> \\server\share\path
    if (grepl("^//[^/]", p)) {
      p <- sub("^//", "\\\\", p)
      p <- chartr("/", "\\", p)
      return(p)
    }
    # Local drive form: "/C:/path" -> "C:/path"
    if (grepl("^/[A-Za-z]:", p)) {
      p <- substring(p, 2L)
    }
    # Normalize separators
    p <- chartr("/", "\\", p)
  }

  p
}

# Internal: resolve/download an artifact into the cache and return a path.
# - Validates inputs with classed errors (writeAlizer_input_error)
# - Honors writeAlizer.mock_dir (returns mock path directly if present)
# - Respects writeAlizer.offline for non-file URLs
# - Re-downloads on cached checksum mismatch; errors if still bad
# - Prints an informative message after a successful download (once)
.wa_ensure_file <- function(file,
                            url,
                            sha256 = NULL,
                            quiet = FALSE,
                            max_retries = 1L) {
  # ---- Input validation (classed) ----
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    rlang::abort("`file` must be a non-empty character scalar.", .subclass = "writeAlizer_input_error")
  }
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    rlang::abort("`url` must be a non-empty character scalar.", .subclass = "writeAlizer_input_error")
  }
  if (!is.null(sha256) && (!is.character(sha256) || length(sha256) != 1L)) {
    rlang::abort("`sha256` must be NULL or a single character string.", .subclass = "writeAlizer_input_error")
  }
  if (!is.numeric(max_retries) || length(max_retries) != 1L || is.na(max_retries) || max_retries < 0) {
    rlang::abort("`max_retries` must be a single non-negative number.", .subclass = "writeAlizer_input_error")
  }

  # ---- Short-circuit to mock_dir if present ----
  mock_dir <- getOption("writeAlizer.mock_dir")
  if (is.character(mock_dir) && nzchar(mock_dir)) {
    mock_path <- file.path(mock_dir, file)
    if (!file.exists(mock_path)) {
      rlang::abort(
        sprintf("Mock artifact not found: %s", mock_path),
        .subclass = "writeAlizer_mock_missing"
      )
    }
    return(normalizePath(mock_path, winslash = "/", mustWork = TRUE))
  }

  dest <- .wa_cached_path(file)

  # Helper: compute sha256 (returns NA_character_ if file missing)
  file_sha256 <- function(path) {
    if (!file.exists(path)) return(NA_character_)
    digest::digest(file = path, algo = "sha256")
  }

  # Helper: verify checksum if requested
  verify_checksum <- function(path, expected) {
    if (is.null(expected) || !nzchar(expected)) return(TRUE)
    got <- file_sha256(path)
    isTRUE(identical(tolower(got), tolower(expected)))
  }

  # If already cached and checksum OK -> return
  if (file.exists(dest)) {
    if (verify_checksum(dest, sha256)) {
      return(normalizePath(dest, winslash = "/", mustWork = TRUE))
    } else {
      # Cached file but checksum mismatch: warn and proceed to re-download
      if (!quiet) {
        warning(sprintf(
          "Checksum mismatch for cached '%s'. Expected %s, got %s. Re-downloading.",
          basename(dest), sha256 %||% "<none>", file_sha256(dest) %||% "<none>"
        ), call. = FALSE)
      }
    }
  }

  # Ensure cache dir exists
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  # Download/copy logic
  do_fetch <- function() {
    # file:// scheme -> local copy
    if (grepl("^file://", url, ignore.case = TRUE)) {
      # Convert file:// URL to local path
      src <- sub("^file:///", "", url)
      src <- sub("^file://",  "", src)  # handle file://C:/... form
      if (!file.exists(src)) {
        rlang::abort(
          sprintf("Missing file for URL '%s'.", url),
          .subclass = "writeAlizer_download_missing"
        )
      }
      # Copy to dest (overwrite)
      file.copy(src, dest, overwrite = TRUE)
    } else {
      # Non-file URL: respect offline
      if (isTRUE(getOption("writeAlizer.offline", FALSE))) {
        rlang::abort(
          sprintf("Cannot download '%s' while offline. Set options(writeAlizer.offline = FALSE) to enable.",
                  basename(file)),
          .subclass = "writeAlizer_offline"
        )
      }
      tmp <- tempfile(fileext = paste0(".", tools::file_ext(file)))
      on.exit(unlink(tmp, force = TRUE), add = TRUE)
      # Use utils::download.file; be quiet to keep tests clean
      utils::download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
      file.rename(tmp, dest)
    }

    # After a successful transfer, emit the friendly message (once)
    if (!quiet) {
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert_info(paste0(
          "Downloaded model artifact:\n",
          "* File: {basename(dest)}\n",
          "* Cache: {wa_cache_dir()}\n",
          "  (Artifacts are downloaded only the first time you use a model.)\n",
          "  Tip: clear the cache with {cli::col_blue('wa_cache_clear()')} if needed."
        ))
      } else {
        message(sprintf(
          "Downloaded model artifact:\n* File: %s\n* Cache: %s\n  (Artifacts are downloaded only the first time you use a model.)\n  Tip: clear the cache with wa_cache_clear() if needed.",
          basename(dest), wa_cache_dir()
        ))
      }
    }

    # Verify checksum (if provided)
    if (!verify_checksum(dest, sha256)) {
      rlang::abort(
        sprintf(
          "Downloaded checksum mismatch for '%s'. Expected %s, got %s.",
          basename(dest),
          sha256 %||% "<none>",
          file_sha256(dest) %||% "<none>"
        ),
        .subclass = "writeAlizer_checksum_mismatch"
      )
    }

    TRUE
  }

  # Try to fetch, with limited retries on mismatch of cached copy
  # (download.file itself will raise errors for connectivity issues)
  tries <- as.integer(max_retries) + 1L
  ok <- FALSE
  last_err <- NULL
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      do_fetch()
    }, error = function(e) {
      last_err <<- e
      FALSE
    })
    if (ok) break
  }
  if (!ok) stop(last_err)

  normalizePath(dest, winslash = "/", mustWork = TRUE)
}

.wa_load_model_rdas <- function(model, envir = parent.frame()) {
  key <- if (exists(".wa_canonical_model", mode = "function")) .wa_canonical_model(model) else model
  mock_dir <- getOption("writeAlizer.mock_dir", NULL)

  if (identical(key, "example") && !is.null(mock_dir)) {
    mock <- file.path(mock_dir, "example.rda")
    if (file.exists(mock)) {
      load(mock, envir = envir)
      return(invisible(TRUE))
    } else {
      rlang::abort(
        "Mock dir is set but 'example.rda' not found; seed it via wa_seed_example_models().",
        .subclass = "writeAlizer_mock_missing"
      )
    }
  }

  parts <- .wa_parts_for(kind = "rda", model = key)
  if (nrow(parts) == 0L) {
    rlang::abort(sprintf("No model artifacts registered for '%s'", model),
                 .subclass = "writeAlizer_parts_missing")
  }

  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]
    mock_candidate <- if (!is.null(mock_dir)) file.path(mock_dir, basename(p$file)) else NULL
    if (!is.null(mock_candidate) && file.exists(mock_candidate)) {
      load(mock_candidate, envir = envir)
    } else {
      load(.wa_ensure_file(p$file, p$url), envir = envir)
    }
  }
  invisible(TRUE)
}

# Load trained model fits (RDA) from cache for a given model key.
# Returns a named list where names are canonicalized from filenames.
.wa_load_fits_list <- function(model) {
  key <- if (exists(".wa_canonical_model", mode = "function")) .wa_canonical_model(model) else model
  mock_dir <- getOption("writeAlizer.mock_dir", NULL)

  # Built-in example model via mock
  if (identical(key, "example") && !is.null(mock_dir)) {
    mock_path <- file.path(mock_dir, "example.rda")
    if (file.exists(mock_path)) {
      tmp  <- new.env(parent = emptyenv())
      objs <- load(mock_path, envir = tmp)
      pick <- if ("fit" %in% objs) "fit" else objs[[1L]]
      fit_obj <- get(pick, envir = tmp, inherits = FALSE)
      if (exists(".wa_require_pkgs_for_fits", mode = "function")) {
        .wa_require_pkgs_for_fits(list(fit_obj))
      }
      return(list("example" = fit_obj))
    } else {
      rlang::abort(
        "Mock dir is set but 'example.rda' not found; seed it via wa_seed_example_models().",
        .subclass = "writeAlizer_mock_missing"
      )
    }
  }

  parts <- .wa_parts_for("rda", key)
  if (nrow(parts) == 0L) {
    rlang::abort(sprintf("No model artifacts registered for '%s'", model),
                 .subclass = "writeAlizer_parts_missing")
  }

  fits <- list()
  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]
    mock_candidate <- if (!is.null(mock_dir)) file.path(mock_dir, basename(p$file)) else NULL
    if (!is.null(mock_candidate) && file.exists(mock_candidate)) {
      path <- mock_candidate
    } else {
      sha  <- if ("sha" %in% names(parts)) parts$sha[i] else NULL
      path <- .wa_ensure_file(p$file, p$url, sha256 = sha)
    }

    tmp  <- new.env(parent = emptyenv())
    objs <- load(path, envir = tmp)

    pick <- NULL
    preferred <- c("fit", "model", "mod", "gbmFit", "glmnet.fit")
    for (cand in preferred) if (cand %in% objs) { pick <- cand; break }
    if (is.null(pick)) pick <- objs[[1L]]

    canonical <- tools::file_path_sans_ext(basename(p$file))
    fits[[canonical]] <- get(pick, envir = tmp, inherits = FALSE)
  }

  if (exists(".wa_require_pkgs_for_fits", mode = "function")) {
    .wa_require_pkgs_for_fits(unname(fits))
  }

  fits
}

.wa_require_pkgs_for_fits <- function(fits) {
  needed <- character(0)

  for (f in fits) {
    cls <- class(f)
    if ("randomForest" %in% cls) needed <- c(needed, "randomForest")
    if ("gbm"          %in% cls) needed <- c(needed, "gbm")
    if ("glmnet"       %in% cls) needed <- c(needed, "glmnet")
    if ("earth"        %in% cls) needed <- c(needed, "earth")
    if ("cubist"       %in% cls || "Cubist" %in% cls) needed <- c(needed, "Cubist")
    if ("ksvm"         %in% cls || "kernlab" %in% cls) needed <- c(needed, "kernlab")
    if ("mvr"          %in% cls || "pls"    %in% cls) needed <- c(needed, "pls")
    if ("caretEnsemble"%in% cls) needed <- c(needed, "caretEnsemble")

    if ("train" %in% cls) {
      libs <- tryCatch({
        mi <- f$modelInfo
        if (!is.null(mi$library)) mi$library else character(0)
      }, error = function(e) character(0))
      needed <- c(needed, libs)
    }
  }

  needed  <- unique(needed)
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing)) {
    rlang::abort(
      paste0(
        "These packages are required to use the loaded model objects but are not installed: ",
        paste(missing, collapse = ", "),
        "\nPlease install them, e.g.: install.packages(c(\"",
        paste(missing, collapse = "\", \""), "\"))"
      ),
      .subclass = "writeAlizer_dependency_missing"
    )
  }

  invisible(TRUE)
}

# Internal: read the artifacts registry (tests may mock/override this)
# Tries an internal helper first if it exists; otherwise reads the CSV shipped in the pkg.
.wa_read_registry <- function() {
  # Prefer an internal accessor if present (lets tests inject a mocked registry)
  if (exists(".wa_artifacts_registry", envir = asNamespace("writeAlizer"), inherits = FALSE)) {
    return(get(".wa_artifacts_registry", envir = asNamespace("writeAlizer"))())
  }
  if (!is.null(getOption("writeAlizer.artifacts_df"))) {
    return(getOption("writeAlizer.artifacts_df"))
  }

  # Fallback: read packaged CSV without requiring readr
  path <- system.file("metadata", "artifacts.csv", package = "writeAlizer")
  utils::read.csv(
    file = path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# Back-compat for tests: return the list of *_vars.rds objects for a model
# Uses the same registry source as the package / mocked tests.
.wa_load_varlists <- function(model) {
  reg <- .wa_read_registry()
  # expected columns in tests: kind, model, part, file, url, sha
  cols <- names(reg)
  has <- function(x) x %in% cols

  rows <- reg[reg$model == model & grepl("_vars\\.rds$", reg$file), , drop = FALSE]
  if (!nrow(rows)) {
    # allow filename prefix fallback if 'model' column isn't matched by the mock
    rows <- reg[grepl(paste0("^", model), reg$file) & grepl("_vars\\.rds$", reg$file), , drop = FALSE]
  }
  if (!nrow(rows)) {
    stop(sprintf("No varlists registered for model '%s'.", model), call. = FALSE)
  }
  if (has("part")) rows <- rows[order(rows$part), , drop = FALSE]

  out <- vector("list", nrow(rows))
  names(out) <- rows$file
  for (i in seq_len(nrow(rows))) {
    p <- rows[i, ]
    sha <- if (has("sha")) p$sha else NULL
    path <- .wa_ensure_file(p$file, p$url, sha256 = sha, quiet = TRUE)
    out[[i]] <- readRDS(path)
  }
  out
}



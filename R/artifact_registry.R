# ------- helpers (internal; do NOT export) -------

.wa_canonical_model <- function(model) {
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    rlang::abort("`model` must be a non-empty character scalar.",
                 .subclass = "writeAlizer_input_error")
  }
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
  csv_opt <- .wa_path_option("writeAlizer.registry_csv")
  if (!is.null(csv_opt)) {
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
  required <- c("kind", "model", "part", "file", "url")
  if (any(vapply(df[required], function(x) anyNA(x) || any(!nzchar(trimws(x))), logical(1))) ||
      any(!df$kind %in% c("rda", "rds")) ||
      anyDuplicated(df[c("kind", "model", "part")])) {
    rlang::abort("artifacts.csv contains missing values, invalid kinds, or duplicate model parts.",
                 .subclass = "writeAlizer_registry_malformed")
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
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) || !nzchar(kind)) {
    rlang::abort("`kind` must be a single string ('rds' or 'rda').",
                 .subclass = "writeAlizer_input_error")
  }
  key <- .wa_canonical_model(model)
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
.wa_from_file_url <- function(url, windows = .Platform$OS.type == "windows") {
  stopifnot(is.character(url), length(url) == 1L, !is.na(url), nzchar(url))
  if (!grepl("^file://", url, ignore.case = TRUE)) return(url)
  p <- utils::URLdecode(sub("^file://", "", url, ignore.case = TRUE))
  p <- sub("^localhost/", "/", p, ignore.case = TRUE)
  if (windows) {
    if (grepl("^/*[A-Za-z]:", p)) {
      p <- sub("^/+", "", p)
    } else if (!startsWith(p, "/") || startsWith(p, "//")) {
      p <- paste0("//", sub("^/+", "", p))
    }
    return(chartr("/", "\\", p))
  }
  paste0("/", sub("^/+", "", p))
}

# Fetch into the destination filesystem, verify, then publish to the cache.
# Mock directories intentionally bypass production checksums for example/test fits.
.wa_ensure_file <- function(file, url, sha256 = NULL, quiet = FALSE, max_retries = 1L) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file) ||
      grepl("^[/\\\\~]|^[A-Za-z]:|(^|[/\\\\])\\.\\.?([/\\\\]|$)", file)) {
    rlang::abort("`file` must be a non-empty relative filename without '.' or '..' path components.",
                 .subclass = "writeAlizer_input_error")
  }
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    rlang::abort("`url` must be a non-empty character scalar.", .subclass = "writeAlizer_input_error")
  }
  # A CSV column containing only missing hashes is read as logical NA by R.
  if (length(sha256) == 1L && is.atomic(sha256) && is.na(sha256)) sha256 <- NULL
  if (!is.null(sha256)) {
    if (!is.character(sha256) || length(sha256) != 1L) {
      rlang::abort("`sha256` must be NULL or a single character string.", .subclass = "writeAlizer_input_error")
    }
    if (is.na(sha256) || !nzchar(sha256)) sha256 <- NULL
  }
  if (!is.null(sha256) && !grepl("^[[:xdigit:]]{64}$", sha256)) {
    rlang::abort("`sha256` must contain 64 hexadecimal characters.", .subclass = "writeAlizer_input_error")
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet) ||
      !is.numeric(max_retries) || length(max_retries) != 1L || !is.finite(max_retries) ||
      max_retries < 0 || max_retries != floor(max_retries) || max_retries >= .Machine$integer.max) {
    rlang::abort("`quiet` must be TRUE or FALSE and `max_retries` a finite non-negative integer.",
                 .subclass = "writeAlizer_input_error")
  }

  mock_dir <- .wa_path_option("writeAlizer.mock_dir")
  if (!is.null(mock_dir)) {
    mock_path <- file.path(mock_dir, file)
    if (!file.exists(mock_path) || dir.exists(mock_path)) {
      rlang::abort(sprintf("Mock artifact not found: %s", mock_path),
                   .subclass = "writeAlizer_mock_missing")
    }
    return(normalizePath(mock_path, winslash = "/", mustWork = TRUE))
  }

  dest <- .wa_cached_path(file)
  file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")
  verify_checksum <- function(path) is.null(sha256) || identical(tolower(file_sha256(path)), tolower(sha256))
  if (file.exists(dest) && !dir.exists(dest)) {
    if (verify_checksum(dest)) return(normalizePath(dest, winslash = "/", mustWork = TRUE))
    if (!quiet) warning(sprintf("Checksum mismatch for cached '%s'. Re-downloading.", basename(dest)), call. = FALSE)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(dest)) || dir.exists(dest)) {
    rlang::abort(sprintf("Cannot write cache file: %s", dest), .subclass = "writeAlizer_download_failed")
  }

  do_fetch <- function() {
    tmp <- tempfile(".wa-download-", tmpdir = dirname(dest))
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    if (grepl("^file://", url, ignore.case = TRUE)) {
      src <- .wa_from_file_url(url)
      if (!file.exists(src) || dir.exists(src)) {
        rlang::abort(sprintf("Missing file for URL '%s'.", url), .subclass = "writeAlizer_download_missing")
      }
      ok <- file.copy(src, tmp, overwrite = TRUE)
    } else {
      if (isTRUE(getOption("writeAlizer.offline", FALSE))) {
        rlang::abort(sprintf("Cannot download '%s' while offline. Set options(writeAlizer.offline = FALSE) to enable.",
                             basename(file)), .subclass = "writeAlizer_offline")
      }
      ok <- identical(utils::download.file(url, destfile = tmp, mode = "wb", quiet = quiet), 0L)
    }
    if (!isTRUE(ok) || !file.exists(tmp)) {
      rlang::abort(sprintf("Failed to download '%s'.", basename(file)), .subclass = "writeAlizer_download_failed")
    }
    if (!verify_checksum(tmp)) {
      rlang::abort(sprintf("Downloaded checksum mismatch for '%s'. Expected %s, got %s.",
                           basename(dest), sha256, file_sha256(tmp)), .subclass = "writeAlizer_checksum_mismatch")
    }
    # Staging beside dest avoids cross-filesystem rename failures.
    if (!file.rename(tmp, dest)) {
      rlang::abort(sprintf("Cannot move downloaded artifact into the cache: %s", dest),
                   .subclass = "writeAlizer_download_failed")
    }
    if (!quiet) cli::cli_alert_info("Downloaded model artifact: {basename(dest)}\nCache: {wa_cache_dir()}")
    TRUE
  }

  last_err <- NULL
  for (i in seq_len(as.integer(max_retries) + 1L)) {
    ok <- tryCatch(do_fetch(), error = function(e) { last_err <<- e; FALSE })
    if (ok) return(normalizePath(dest, winslash = "/", mustWork = TRUE))
  }
  stop(last_err)
}

.wa_load_model_rdas <- function(model, envir = parent.frame()) {
  key <- .wa_canonical_model(model)
  mock_dir <- .wa_path_option("writeAlizer.mock_dir")

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
      load(.wa_ensure_file(p$file, p$url, sha256 = p$sha), envir = envir)
    }
  }
  invisible(TRUE)
}

# Load trained model fits (RDA) from cache for a given model key.
# Returns a named list where names are canonicalized from filenames.
.wa_load_fits_list <- function(model) {
  key <- .wa_canonical_model(model)
  mock_dir <- .wa_path_option("writeAlizer.mock_dir")

  # Built-in example model via mock
  if (identical(key, "example") && !is.null(mock_dir)) {
    mock_path <- file.path(mock_dir, "example.rda")
    if (file.exists(mock_path)) {
      tmp  <- new.env(parent = emptyenv())
      objs <- load(mock_path, envir = tmp)
      .wa_check_archive(objs, mock_path)
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
    .wa_check_archive(objs, path)

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

.wa_check_archive <- function(objects, path) {
  if (!length(objects)) {
    rlang::abort(paste0("Model artifact contains no objects: ", path),
                 .subclass = "writeAlizer_artifact_missing")
  }
  invisible(TRUE)
}

.wa_require_pkgs_for_fits <- function(fits) {
  needed <- character(0)

  for (f in fits) {
    if (inherits(f, "caretEnsemble") || inherits(f, "caretStack")) {
      .wa_require_pkgs_for_fits(c(f$models, list(f$ens_model)))
    }
    cls <- class(f)
    if ("randomForest" %in% cls) needed <- c(needed, "randomForest")
    if ("gbm"          %in% cls) needed <- c(needed, "gbm")
    if ("glmnet"       %in% cls) needed <- c(needed, "glmnet")
    if ("earth"        %in% cls) needed <- c(needed, "earth")
    if ("cubist"       %in% cls || "Cubist" %in% cls) needed <- c(needed, "Cubist")
    if ("ksvm"         %in% cls || "kernlab" %in% cls) needed <- c(needed, "kernlab")
    if ("mvr"          %in% cls || "pls"    %in% cls) needed <- c(needed, "pls")
    if (any(c("caretEnsemble", "caretStack") %in% cls)) needed <- c(needed, "caretEnsemble")

    if ("train" %in% cls) {
      libs <- tryCatch({
        mi <- f$modelInfo
        if (!is.null(mi$library)) mi$library else character(0)
      }, error = function(e) character(0))
      needed <- c(needed, libs)
    }
  }

  needed  <- unique(needed[!is.na(needed) & nzchar(needed)])
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

  .wa_registry()
}

# Back-compat for tests: return the list of *_vars.rds objects for a model
# Uses the same registry source as the package / mocked tests.
.wa_load_varlists <- function(model) {
  model <- .wa_canonical_model(model)
  reg <- .wa_read_registry()
  # expected columns in tests: kind, model, part, file, url, sha
  cols <- names(reg)
  has <- function(x) x %in% cols

  rows <- reg[reg$model == model & grepl("_vars(_v2)?\\.rds$", reg$file), , drop = FALSE]
  if (!nrow(rows)) {
    # allow filename prefix fallback if 'model' column isn't matched by the mock
    rows <- reg[startsWith(reg$file, model) & grepl("_vars(_v2)?\\.rds$", reg$file), , drop = FALSE]
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


# Internal utilities for centralizing model artifacts (files & URLs).

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
  csv <- system.file("metadata", "artifacts.csv", package = "writeAlizer")
  if (!nzchar(csv) || !file.exists(csv)) {
    stop(
      "Registry CSV not found (inst/metadata/artifacts.csv). ",
      "Reinstall writeAlizer or ensure the file is included in the package.",
      call. = FALSE
    )
  }
  df <- utils::read.csv(csv, stringsAsFactors = FALSE)
  need <- c("kind","model","part","file","url","sha")
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    stop(
      "artifacts.csv missing columns: ", paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  df
}

# Helper to filter registry by kind/model and return rows (including sha)
.wa_parts_for <- function(kind, model) {
  reg <- .wa_registry()
  if (!all(c("kind","model","part","file","url") %in% names(reg))) {
    stop("artifacts registry is missing required columns.", call. = FALSE)
  }
  if (!is.character(kind) || length(kind) != 1L) {
    stop("`kind` must be a single string ('rds' or 'rda').", call. = FALSE)
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

# Internal: ensure an artifact is present in the user cache.
# - Honors options(writeAlizer.mock_dir) as an override (used in tests/examples)
# - Verifies checksum when provided (with warnings on mismatch)
# - Fails gracefully with an informative message if offline / remote unavailable
.wa_ensure_file <- function(file, url, sha256 = NULL, quiet = TRUE) {
  stopifnot(is.character(file), length(file) == 1L, nzchar(file),
            is.character(url),  length(url)  == 1L, nzchar(url))

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Preserve subdirectory structure under the cache
  dest <- file.path(cache_dir, file)
  dest_dir <- dirname(dest)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # 1) mock_dir override (used by tests/examples to avoid network)
  mock_dir <- getOption("writeAlizer.mock_dir", NULL)
  if (is.character(mock_dir) && nzchar(mock_dir)) {
    mock_path <- file.path(mock_dir, file)  # preserve subdirs here, too
    if (file.exists(mock_path)) {
      if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      ok <- tryCatch({
        file.copy(mock_path, dest, overwrite = TRUE)
      }, warning = function(w) TRUE, error = function(e) FALSE)
      if (isTRUE(ok) && file.exists(dest)) return(dest)
    }
  }

  # 2) Use a valid cached copy if present (and checksum matches, if provided)
  if (file.exists(dest)) {
    if (is.null(sha256)) return(dest)
    got <- tryCatch(digest::digest(dest, algo = "sha256", file = TRUE),
                    error = function(e) NA_character_)
    if (!is.na(got) && identical(got, sha256)) return(dest)
    # checksum mismatch -> warn and re-download
    warning(paste0(
      "Checksum mismatch for cached '", basename(file), "'. ",
      "Expected ", sha256, ", got ", got, ". Re-downloading."
    ), call. = FALSE)
    unlink(dest, force = TRUE)
  }

  # 3) If this is a remote URL and we're offline, fail gracefully & informatively
  is_remote <- !startsWith(tolower(url), "file:")
  offline_flag <- isTRUE(getOption("writeAlizer.offline", FALSE))
  has_curl <- isTRUE(requireNamespace("curl", quietly = TRUE))
  offline_now <- has_curl && is_remote && (!curl::has_internet() || offline_flag)

  if (offline_now) {
    msg <- paste0(
      "Network resource unavailable. Could not download '", basename(file), "' from\n  ",
      url, "\n\n",
      "To run offline, either:\n",
      "  * Set options(writeAlizer.mock_dir = <dir>) with local copies of artifacts, or\n",
      "  * Use wa_seed_example_models('example') for offline examples.\n"
    )
    stop(msg, call. = FALSE)
  }

  # 4) Download (supports file:// URLs too)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  utils::download.file(url = url, destfile = dest, mode = "wb", quiet = quiet)

  if (!file.exists(dest)) {
    stop("Download reported success but the file is missing: ", dest, call. = FALSE)
  }

  # 5) Optional checksum verification (warn + error on mismatch)
  if (!is.null(sha256)) {
    got <- digest::digest(dest, algo = "sha256", file = TRUE)
    if (!identical(got, sha256)) {
      warning(paste0(
        "Checksum mismatch after download for '", basename(file), "'. ",
        "Expected ", sha256, ", got ", got, "."
      ), call. = FALSE)
      unlink(dest, force = TRUE)
      stop(
        "Checksum mismatch for '", basename(file), "'.\n",
        "  expected: ", sha256, "\n",
        "  got:      ", got, "\n",
        "Please try again later or contact the maintainer.", call. = FALSE
      )
    }
  }

  dest
}

# Optional convenience loaders used by our refactor
.wa_load_varlists <- function(model) {
  parts <- .wa_parts_for(kind = "rds", model = model)
  if (nrow(parts) == 0L) stop(sprintf("No variable lists registered for model '%s'", model))
  lapply(seq_len(nrow(parts)), function(i) {
    p <- parts[i, ]
    readRDS(.wa_ensure_file(p$file, p$url))
  })
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
      stop("Mock dir is set but 'example.rda' not found; seed it via wa_seed_example_models().", call. = FALSE)
    }
  }

  parts <- .wa_parts_for(kind = "rda", model = key)
  if (nrow(parts) == 0L) stop(sprintf("No model artifacts registered for '%s'", model))

  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]
    ## NEW: if a mock with the same filename exists, use it instead of downloading
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
  # Canonicalize key and check for mocks
  key <- if (exists(".wa_canonical_model", mode = "function")) .wa_canonical_model(model) else model
  mock_dir <- getOption("writeAlizer.mock_dir", NULL)

  ## short-circuit for the built-in example model
  if (identical(key, "example") && !is.null(mock_dir)) {
    mock_path <- file.path(mock_dir, "example.rda")
    if (file.exists(mock_path)) {
      tmp  <- new.env(parent = emptyenv())
      objs <- load(mock_path, envir = tmp)
      pick <- if ("fit" %in% objs) "fit" else objs[[1L]]
      fit_obj <- get(pick, envir = tmp, inherits = FALSE)

      # Dependency check on the single fit (reuses existing helper)
      if (exists(".wa_require_pkgs_for_fits", mode = "function")) {
        .wa_require_pkgs_for_fits(list(fit_obj))
      }
      return(list("example" = fit_obj))
    } else {
      stop("Mock dir is set but 'example.rda' not found; seed it via wa_seed_example_models().", call. = FALSE)
    }
  }

  # use registry to discover artifacts
  parts <- .wa_parts_for("rda", key)
  if (nrow(parts) == 0L) {
    stop(sprintf("No model artifacts registered for '%s'", model), call. = FALSE)
  }

  fits <- list()
  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]

    ## if a mock with the same filename exists, use it; else ensure/download
    mock_candidate <- if (!is.null(mock_dir)) file.path(mock_dir, basename(p$file)) else NULL
    if (!is.null(mock_candidate) && file.exists(mock_candidate)) {
      path <- mock_candidate
    } else {
      sha  <- if ("sha" %in% names(parts)) parts$sha[i] else NULL
      path <- .wa_ensure_file(p$file, p$url, sha256 = sha)
    }

    # Load into a throwaway environment and pick a sensible object
    tmp  <- new.env(parent = emptyenv())
    objs <- load(path, envir = tmp)

    pick <- NULL
    preferred <- c("fit", "model", "mod", "gbmFit", "glmnet.fit")
    for (cand in preferred) {
      if (cand %in% objs) { pick <- cand; break }
    }
    if (is.null(pick)) pick <- objs[[1L]]

    canonical <- tools::file_path_sans_ext(basename(p$file))
    fits[[canonical]] <- get(pick, envir = tmp, inherits = FALSE)
  }

  # Ensure any required packages for these model objects are available
  if (exists(".wa_require_pkgs_for_fits", mode = "function")) {
    .wa_require_pkgs_for_fits(unname(fits))
  }

  fits
}

.wa_require_pkgs_for_fits <- function(fits) {
  needed <- character(0)

  for (f in fits) {
    cls <- class(f)

    # Direct model classes → packages
    if ("randomForest" %in% cls) needed <- c(needed, "randomForest")
    if ("gbm"          %in% cls) needed <- c(needed, "gbm")
    if ("glmnet"       %in% cls) needed <- c(needed, "glmnet")
    if ("earth"        %in% cls) needed <- c(needed, "earth")
    if ("cubist"       %in% cls || "Cubist" %in% cls) needed <- c(needed, "Cubist")
    if ("ksvm"         %in% cls || "kernlab" %in% cls) needed <- c(needed, "kernlab")
    if ("mvr"          %in% cls || "pls"    %in% cls) needed <- c(needed, "pls")
    if ("caretEnsemble"%in% cls) needed <- c(needed, "caretEnsemble")

    # caret::train objects often require additional libraries specified in modelInfo
    if ("train" %in% cls) {
      libs <- tryCatch(
        {
          mi <- f$modelInfo
          if (!is.null(mi$library)) mi$library else character(0)
        },
        error = function(e) character(0)
      )
      needed <- c(needed, libs)
    }
  }

  needed <- unique(needed)
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing)) {
    stop(
      paste0(
        "These packages are required to use the loaded model objects but are not installed: ",
        paste(missing, collapse = ", "),
        "\nPlease install them, e.g.: install.packages(c(\"",
        paste(missing, collapse = "\", \""), "\"))"
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


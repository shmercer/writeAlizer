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
    rlang::abort(
      c(
        "Registry CSV not found (inst/metadata/artifacts.csv)." =
          "Reinstall writeAlizer or ensure the file is included in the package."
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

# Ensure a cached artifact exists (internal)
# Internal helper that ensures an artifact is present in the writeAlizer cache.
.wa_ensure_file <- function(file, url, sha256 = NULL, quiet = TRUE,
                            max_retries = 3L, base_sleep = 0.5) {
  if (!is.character(file) || length(file) != 1L || !nzchar(file) ||
      !is.character(url)  || length(url)  != 1L || !nzchar(url)) {
    rlang::abort("`file` and `url` must be non-empty character scalars.",
                 .subclass = "writeAlizer_input_error")
  }

  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  dest <- file.path(cache_dir, file)
  dest_dir <- dirname(dest)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # 1) mock_dir override (used by tests/examples to avoid network)
  mock_dir <- getOption("writeAlizer.mock_dir", NULL)
  if (is.character(mock_dir) && nzchar(mock_dir)) {
    mock_path <- file.path(mock_dir, file)
    if (file.exists(mock_path)) {
      ok <- tryCatch({ file.copy(mock_path, dest, overwrite = TRUE) },
                     warning = function(w) TRUE, error = function(e) FALSE)
      if (isTRUE(ok) && file.exists(dest)) return(dest)
    }
  }

  # 2) Cached copy (verify checksum when provided)
  if (file.exists(dest)) {
    if (is.null(sha256)) return(dest)
    got <- tryCatch(digest::digest(dest, algo = "sha256", file = TRUE),
                    error = function(e) NA_character_)
    if (!is.na(got) && identical(got, sha256)) return(dest)
    warning(sprintf("Checksum mismatch for cached '%s'. Expected %s, got %s. Re-downloading.",
                    basename(file), sha256, got), call. = FALSE)
    unlink(dest, force = TRUE)
  }

  # 3) Respect offline
  if (isTRUE(getOption("writeAlizer.offline", FALSE))) {
    rlang::abort(
      paste0("Cannot download '", basename(file),
             "' while offline. Set options(writeAlizer.offline = FALSE) to enable."),
      .subclass = "writeAlizer_offline_error"
    )
  }

  # 4) Download with retry + backoff; support file:// URLs
  attempt <- 0L
  last_error <- NULL
  while (attempt <= max_retries) {
    attempt <- attempt + 1L
    ok <- FALSE
    tryCatch({
      if (startsWith(url, "file://")) {
        src <- sub("^file://", "", url)
        if (!file.exists(src)) {
          rlang::abort(sprintf("Local artifact not found at '%s'.", src),
                       .subclass = "writeAlizer_artifact_missing")
        }
        ok <- file.copy(src, dest, overwrite = TRUE)
      } else {
        utils::download.file(url, dest, quiet = quiet, mode = "wb", cacheOK = TRUE)
        ok <- file.exists(dest)
      }
    }, error = function(e) {
      last_error <<- e
      ok <<- FALSE
    }, warning = function(w) {
      # treat warnings (e.g., HTTP issues) as retryable errors
      last_error <<- w
      ok <<- FALSE
    })

    if (ok && file.exists(dest)) {
      # Optional checksum verify
      if (!is.null(sha256)) {
        got <- tryCatch(digest::digest(dest, algo = "sha256", file = TRUE),
                        error = function(e) NA_character_)
        if (!is.na(got) && identical(got, sha256)) return(dest)
        # bad checksum; delete and retry
        unlink(dest, force = TRUE)
        last_error <- simpleError(
          sprintf("Checksum mismatch after download. Expected %s, got %s.", sha256, got)
        )
      } else {
        return(dest)
      }
    }

    if (attempt <= max_retries) {
      Sys.sleep(base_sleep * (2 ^ (attempt - 1L)))
    }
  }

  # Give up with a typed error
  rlang::abort(
    paste0("Failed to download '", basename(file), "' from ", url, " after ",
           max_retries + 1L, " attempts: ", conditionMessage(last_error %||% simpleError("unknown error"))),
    .subclass = "writeAlizer_download_error"
  )
}


.wa_load_varlists <- function(model) {
  parts <- .wa_parts_for(kind = "rds", model = model)
  if (nrow(parts) == 0L) {
    rlang::abort(sprintf("No variable lists registered for model '%s'", model),
                 .subclass = "writeAlizer_parts_missing")
  }
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

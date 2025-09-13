# Internal utilities for centralizing model artifacts (files & URLs).

# ------- helpers (internal; do NOT export) -------

.wa_canonical_model <- function(model) {
  switch(model,
         "rb_mod3narr" = "rb_mod3narr_v2",
         "rb_mod3exp"  = "rb_mod3exp_v2",
         "rb_mod3per"  = "rb_mod3per_v2",
         "rb_mod3all"  = "rb_mod3all_v2",
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

# Internal helper: ensure an artifact exists in the user cache.
# - Downloads when missing or when checksum fails.
# - Verifies SHA-256 if provided (string of 64 hex chars).
# - Returns the absolute path to the cached file.
.wa_ensure_file <- function(file, url, sha256 = NULL, quiet = FALSE,
                            overwrite = FALSE, retries = 1L) {
  # Resolve cache path (use .wa_cached_path() if present; else fallback to R_user_dir)
  dest <- if (exists(".wa_cached_path", mode = "function")) {
    .wa_cached_path(file)
  } else {
    dir <- tools::R_user_dir("writeAlizer", "cache")
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.path(dir, file)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  # Normalise expected checksum if provided
  .norm_sha <- function(x) tolower(trimws(as.character(x)))
  expected <- if (is.null(sha256)) NULL else .norm_sha(sha256)

  # SHA256 verifier
  .verify_sha <- function(path, expected) {
    if (is.null(expected)) return(TRUE)
    got <- tryCatch(digest::digest(path, algo = "sha256", file = TRUE),
                    error = function(e) NA_character_)
    identical(.norm_sha(got), expected)
  }

  # If we already have a file and either no checksum is provided or it matches, we're done
  if (file.exists(dest) && !overwrite) {
    if (.verify_sha(dest, expected)) {
      return(dest)
    } else {
      warning(sprintf("Checksum mismatch in cache for '%s'; re-downloading.", basename(dest)))
      try(unlink(dest, force = TRUE), silent = TRUE)
    }
  }

  # Guard: need a URL to download if missing or invalid
  if (!nzchar(url)) {
    stop(sprintf("No URL provided to fetch artifact '%s'.", file), call. = FALSE)
  }

  # Download with simple retry loop
  tries <- max(1L, as.integer(retries))
  last_err <- NULL
  for (i in seq_len(tries)) {
    # Clean any partials before each attempt
    if (file.exists(dest)) try(unlink(dest, force = TRUE), silent = TRUE)

    # Use binary mode for portability
    ok <- tryCatch({
      utils::download.file(url = url, destfile = dest, mode = "wb", quiet = quiet)
      TRUE
    }, error = function(e) {
      last_err <<- e
      FALSE
    })

    if (!ok) {
      next
    }

    # Verify checksum if expected
    if (.verify_sha(dest, expected)) {
      return(dest)
    } else {
      last_err <- simpleError(sprintf(
        "Checksum mismatch after download for '%s' (expected %s).",
        basename(dest), if (is.null(expected)) "<none>" else expected
      ))
    }
  }

  # If we get here, all attempts failed
  # Clean any bad partial
  if (file.exists(dest)) try(unlink(dest, force = TRUE), silent = TRUE)
  if (!is.null(last_err)) stop(last_err)
  stop(sprintf("Failed to ensure artifact '%s' in cache.", file), call. = FALSE)
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
  parts <- .wa_parts_for(kind = "rda", model = model)
  if (nrow(parts) == 0L) stop(sprintf("No model artifacts registered for '%s'", model))
  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]
    load(.wa_ensure_file(p$file, p$url), envir = envir)
  }
  invisible(TRUE)
}

# Load trained model fits (RDA) from cache for a given model key.
# Returns a named list where names are canonicalized from filenames (e.g., "rb_mod1a").
.wa_load_fits_list <- function(model) {
  # Map legacy keys (e.g., rb_mod3narr -> rb_mod3narr_v2), if mapping helper exists
  key <- if (exists(".wa_canonical_model", mode = "function")) .wa_canonical_model(model) else model

  parts <- .wa_parts_for("rda", key)
  if (nrow(parts) == 0L) {
    stop(sprintf("No model artifacts registered for '%s'", model), call. = FALSE)
  }

  fits <- list()
  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]

    # Ensure .rda file exists in user cache (download + optional checksum)
    sha  <- if ("sha" %in% names(parts)) parts$sha[i] else NULL
    path <- .wa_ensure_file(p$file, p$url, sha256 = sha)

    # Load into a throwaway environment and pick a sensible object
    tmp  <- new.env(parent = emptyenv())
    objs <- load(path, envir = tmp)

    pick <- NULL
    preferred <- c("fit", "model", "mod", "gbmFit", "glmnet.fit")
    for (cand in preferred) {
      if (cand %in% objs) { pick <- cand; break }
    }
    if (is.null(pick)) pick <- objs[[1L]]

    canonical <- tools::file_path_sans_ext(basename(p$file))  # e.g., "rb_mod1a"
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


# Internal utilities for centralizing model artifacts (files & URLs).
# Keep this file data-driven so adding a new model is a one-line edit.

# Table of artifacts: kind ("rds" varlists, "rda" model files), model key, part, file name, URL.
.wa_artifacts <- function() {
  data.frame(
    kind  = c(
      # -------- RDS (variable lists) ----------
      # rb_mod2
      "rds","rds","rds",
      # rb_mod3 v2 single-genre
      "rds","rds","rds",
      # rb_mod3 v2 all
      "rds","rds","rds",
      # coh_mod2
      "rds","rds","rds",
      # coh_mod3 single-genre
      "rds","rds","rds",
      # coh_mod3 all
      "rds","rds","rds",

      # -------- RDA (trained models) ----------
      # rb_mod1 (6 parts)
      "rda","rda","rda","rda","rda","rda",
      # rb_mod2 (3 parts)
      "rda","rda","rda",
      # rb_mod3 v2 single-genre
      "rda","rda","rda",
      # rb_mod3 v2 all (bundle the three v2 fits)
      "rda","rda","rda",
      # coh_mod1 (6 parts)
      "rda","rda","rda","rda","rda","rda",
      # coh_mod2 (3 parts)
      "rda","rda","rda",
      # coh_mod3 single-genre
      "rda","rda","rda",
      # coh_mod3 all
      "rda","rda","rda",
      # gamet_cws1 (2 parts)
      "rda","rda"
    ),
    model = c(
      # -------- RDS ----------
      # rb_mod2
      "rb_mod2","rb_mod2","rb_mod2",
      # rb_mod3 v2 single-genre
      "rb_mod3narr_v2","rb_mod3exp_v2","rb_mod3per_v2",
      # rb_mod3 v2 all
      "rb_mod3all_v2","rb_mod3all_v2","rb_mod3all_v2",
      # coh_mod2
      "coh_mod2","coh_mod2","coh_mod2",
      # coh_mod3 single-genre
      "coh_mod3narr","coh_mod3exp","coh_mod3per",
      # coh_mod3 all
      "coh_mod3all","coh_mod3all","coh_mod3all",

      # -------- RDA ----------
      # rb_mod1
      "rb_mod1","rb_mod1","rb_mod1","rb_mod1","rb_mod1","rb_mod1",
      # rb_mod2
      "rb_mod2","rb_mod2","rb_mod2",
      # rb_mod3 v2 single-genre
      "rb_mod3narr_v2","rb_mod3exp_v2","rb_mod3per_v2",
      # rb_mod3 v2 all (same three v2 fits)
      "rb_mod3all_v2","rb_mod3all_v2","rb_mod3all_v2",
      # coh_mod1
      "coh_mod1","coh_mod1","coh_mod1","coh_mod1","coh_mod1","coh_mod1",
      # coh_mod2
      "coh_mod2","coh_mod2","coh_mod2",
      # coh_mod3 single-genre
      "coh_mod3narr","coh_mod3exp","coh_mod3per",
      # coh_mod3 all
      "coh_mod3all","coh_mod3all","coh_mod3all",
      # gamet_cws1
      "gamet_cws1","gamet_cws1"
    ),
    part  = c(
      # -------- RDS ----------
      "a","b","c",  # rb_mod2
      "a","a","a",  # rb_mod3 v2 single-genre
      "a","b","c",  # rb_mod3 v2 all
      "a","b","c",  # coh_mod2
      "a","a","a",  # coh_mod3 single-genre
      "a","b","c",  # coh_mod3 all

      # -------- RDA ----------
      "a","b","c","d","e","f",  # rb_mod1
      "a","b","c",              # rb_mod2
      "a","a","a",              # rb_mod3 v2 single-genre
      "a","b","c",              # rb_mod3 v2 all
      "a","b","c","d","e","f",  # coh_mod1
      "a","b","c",              # coh_mod2
      "a","a","a",              # coh_mod3 single-genre
      "a","b","c",              # coh_mod3 all
      "a","b"                   # gamet_cws1
    ),
    file  = c(
      # -------- RDS ----------
      "rb_mod2a_vars.rds","rb_mod2b_vars.rds","rb_mod2c_vars.rds",
      "rb_narr_vars_v2.rds","rb_exp_vars_v2.rds","rb_per_vars_v2.rds",
      "rb_exp_vars_v2.rds","rb_narr_vars_v2.rds","rb_per_vars_v2.rds",
      "coh_mod2a_vars.rds","coh_mod2b_vars.rds","coh_mod2c_vars.rds",
      "coh_narr_vars.rds","coh_exp_vars.rds","coh_per_vars.rds",
      "coh_exp_vars.rds","coh_narr_vars.rds","coh_per_vars.rds",

      # -------- RDA ----------
      "rb_mod1a.rda","rb_mod1b.rda","rb_mod1c.rda","rb_mod1d.rda","rb_mod1e.rda","rb_mod1f.rda",
      "rb_mod2a.rda","rb_mod2b.rda","rb_mod2c.rda",
      "rb_mod3narr_v2.rda","rb_mod3exp_v2.rda","rb_mod3per_v2.rda",
      "rb_mod3exp_v2.rda","rb_mod3narr_v2.rda","rb_mod3per_v2.rda",
      "coh_mod1a.rda","coh_mod1b.rda","coh_mod1c.rda","coh_mod1d.rda","coh_mod1e.rda","coh_mod1f.rda",
      "coh_mod2a.rda","coh_mod2b.rda","coh_mod2c.rda",
      "coh_mod3narr.rda","coh_mod3exp.rda","coh_mod3per.rda",
      "coh_mod3exp.rda","coh_mod3narr.rda","coh_mod3per.rda",
      "CWS_mod1a.rda","CIWS_mod1a.rda"
    ),
    url   = c(
      # -------- RDS ----------
      "https://osf.io/2rsnc/download","https://osf.io/qjg68/download","https://osf.io/kqdvt/download",
      "https://osf.io/8v6nz/download","https://osf.io/gvtyx/download","https://osf.io/7dhc6/download",
      "https://osf.io/gvtyx/download","https://osf.io/8v6nz/download","https://osf.io/7dhc6/download",
      "https://osf.io/qp7fc/download","https://osf.io/upn6j/download","https://osf.io/8qmzv/download",
      "https://osf.io/rbg9n/download","https://osf.io/v5wf3/download","https://osf.io/ekrgu/download",
      "https://osf.io/v5wf3/download","https://osf.io/rbg9n/download","https://osf.io/ekrgu/download",

      # -------- RDA ----------
      "https://osf.io/eq9rw/download","https://osf.io/sy4dw/download","https://osf.io/64dxf/download",
      "https://osf.io/5yghv/download","https://osf.io/kgxtu/download","https://osf.io/5wdet/download",
      "https://osf.io/bpzhs/download","https://osf.io/vzkhn/download","https://osf.io/cqkrv/download",
      "https://osf.io/rqtzm/download","https://osf.io/hknxf/download","https://osf.io/ntgfm/download",
      "https://osf.io/hknxf/download","https://osf.io/rqtzm/download","https://osf.io/ntgfm/download",
      "https://osf.io/qws5x/download","https://osf.io/rdmfw/download","https://osf.io/dq2s9/download",
      "https://osf.io/be6qv/download","https://osf.io/pv3gm/download","https://osf.io/myk6f/download",
      "https://osf.io/mr7kg/download","https://osf.io/zxhcu/download","https://osf.io/n6hqg/download",
      "https://osf.io/y5hjz/download","https://osf.io/6x95q/download","https://osf.io/vrnt9/download",
      "https://osf.io/6x95q/download","https://osf.io/y5hjz/download","https://osf.io/vrnt9/download",
      "https://osf.io/tfw95/download","https://osf.io/yjuxn/download"
    ),
    stringsAsFactors = FALSE
  )
}

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

# Read the shipped CSV registry (preferred), or fall back to the in-code table.
.wa_registry <- function() {
  csv <- system.file("metadata", "artifacts.csv", package = "writeAlizer")
  if (nzchar(csv) && file.exists(csv)) {
    df <- utils::read.csv(csv, stringsAsFactors = FALSE)
    need <- c("kind","model","part","file","url","sha")
    miss <- setdiff(need, names(df))
    if (length(miss)) stop("artifacts.csv missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
    return(df)
  }
  if (exists(".wa_artifacts", mode = "function")) {
    df <- .wa_artifacts()
    if (!"sha" %in% names(df)) df$sha <- NA_character_
    return(df)
  }
  stop("No registry found (inst/metadata/artifacts.csv or .wa_artifacts()).", call. = FALSE)
}

# Filter registry rows by kind and model
.wa_parts_for <- function(kind, model) {
  reg <- .wa_registry()
  reg[reg$kind == kind & reg$model == model, , drop = FALSE]
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


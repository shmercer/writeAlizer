# Internal utilities for centralizing model artifacts (files & URLs).
# Keep this file data-driven so adding a new model is a one-line edit.

# Table of artifacts: kind ("rds" varlists, "rda" model files), model key, part, file name, URL.
.wa_artifacts <- function() {
  data.frame(
    kind  = c(
      # -------- RDS (variable lists) ----------
      # rb_mod2
      "rds","rds","rds",
      # rb_mod3 single-genre
      "rds","rds","rds",
      # rb_mod3 all
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
      # rb_mod3 single-genre
      "rda","rda","rda",
      # rb_mod3 all
      "rda","rda","rda",
      # rb_mod3 v2 single-genre
      "rda","rda","rda",
      # rb_mod3 v2 all
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
      # rb_mod3 single-genre
      "rb_mod3narr","rb_mod3exp","rb_mod3per",
      # rb_mod3 all
      "rb_mod3all","rb_mod3all","rb_mod3all",
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
      # rb_mod3 single-genre
      "rb_mod3narr","rb_mod3exp","rb_mod3per",
      # rb_mod3 all
      "rb_mod3all","rb_mod3all","rb_mod3all",
      # rb_mod3 v2 single-genre
      "rb_mod3narr_v2","rb_mod3exp_v2","rb_mod3per_v2",
      # rb_mod3 v2 all
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
      "a","a","a",  # rb_mod3 single-genre
      "a","b","c",  # rb_mod3 all
      "a","a","a",  # rb_mod3 v2 single-genre
      "a","b","c",  # rb_mod3 v2 all
      "a","b","c",  # coh_mod2
      "a","a","a",  # coh_mod3 single-genre
      "a","b","c",  # coh_mod3 all

      # -------- RDA ----------
      "a","b","c","d","e","f",  # rb_mod1
      "a","b","c",              # rb_mod2
      "a","a","a",              # rb_mod3 single-genre
      "a","b","c",              # rb_mod3 all
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
      "rb_mod3narr_vars.rds","rb_mod3exp_vars.rds","rb_mod3per_vars.rds",
      "rb_mod3exp_vars.rds","rb_mod3narr_vars.rds","rb_mod3per_vars.rds",
      "rb_narr_vars_v2.rds","rb_exp_vars_v2.rds","rb_per_vars_v2.rds",
      "rb_exp_vars_v2.rds","rb_narr_vars_v2.rds","rb_per_vars_v2.rds",
      "coh_mod2a_vars.rds","coh_mod2b_vars.rds","coh_mod2c_vars.rds",
      "coh_narr_vars.rds","coh_exp_vars.rds","coh_per_vars.rds",
      "coh_exp_vars.rds","coh_narr_vars.rds","coh_per_vars.rds",

      # -------- RDA ----------
      "rb_mod1a.rda","rb_mod1b.rda","rb_mod1c.rda","rb_mod1d.rda","rb_mod1e.rda","rb_mod1f.rda",
      "rb_mod2a.rda","rb_mod2b.rda","rb_mod2c.rda",
      "rb_mod3narr.rda","rb_mod3exp.rda","rb_mod3per.rda",
      "rb_mod3exp.rda","rb_mod3narr.rda","rb_mod3per.rda",
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
      "https://osf.io/bmqg9/download","https://osf.io/8ut7r/download","https://osf.io/8mgcn/download",
      "https://osf.io/8ut7r/download","https://osf.io/bmqg9/download","https://osf.io/8mgcn/download",
      "https://osf.io/8v6nz/download","https://osf.io/gvtyx/download","https://osf.io/7dhc6/download",
      "https://osf.io/gvtyx/download","https://osf.io/8v6nz/download","https://osf.io/7dhc6/download",
      "https://osf.io/qp7fc/download","https://osf.io/upn6j/download","https://osf.io/8qmzv/download",
      "https://osf.io/rbg9n/download","https://osf.io/v5wf3/download","https://osf.io/ekrgu/download",
      "https://osf.io/v5wf3/download","https://osf.io/rbg9n/download","https://osf.io/ekrgu/download",

      # -------- RDA ----------
      "https://osf.io/eq9rw/download","https://osf.io/sy4dw/download","https://osf.io/64dxf/download",
      "https://osf.io/5yghv/download","https://osf.io/kgxtu/download","https://osf.io/5wdet/download",
      "https://osf.io/bpzhs/download","https://osf.io/vzkhn/download","https://osf.io/cqkrv/download",
      "https://osf.io/f4nhu/download","https://osf.io/rx6aj/download","https://osf.io/kqxte/download",
      "https://osf.io/rx6aj/download","https://osf.io/f4nhu/download","https://osf.io/kqxte/download",
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

.wa_parts_for <- function(kind, model) {
  af <- .wa_artifacts()
  af[af$kind == kind & af$model == model, , drop = FALSE]
}

.wa_local_path <- function(filename) {
  file.path(system.file("extdata", package = "writeAlizer"), filename)
}

# Uses your existing exported download(file, url) to write under inst/extdata.
# Honors an override option for tests so we never hit the network.
.wa_ensure_file <- function(filename, url) {
  # Test override: if option writeAlizer.mock_dir is set and file exists there, use it
  mock_dir <- getOption("writeAlizer.mock_dir", default = NULL)
  if (!is.null(mock_dir)) {
    cand <- file.path(mock_dir, filename)
    if (file.exists(cand)) return(cand)
  }

  # Normal behavior: use package extdata
  dest <- .wa_local_path(filename)
  if (!file.exists(dest)) {
    download(filename, url)
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
  parts <- .wa_parts_for(kind = "rda", model = model)
  if (nrow(parts) == 0L) stop(sprintf("No model artifacts registered for '%s'", model))
  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]
    load(.wa_ensure_file(p$file, p$url), envir = envir)
  }
  invisible(TRUE)
}

# Load trained model fits for a given model key and return them as a named list.
# Names are derived from filenames (e.g., "rb_mod1a.rda" -> "rb_mod1a").
.wa_load_fits_list <- function(model) {
  parts <- .wa_parts_for("rda", model)
  if (nrow(parts) == 0L) stop(sprintf("No model artifacts registered for '%s'", model))

  fits <- list()
  for (i in seq_len(nrow(parts))) {
    p <- parts[i, ]
    path <- .wa_ensure_file(p$file, p$url)

    tmp <- new.env(parent = emptyenv())
    objs <- load(path, envir = tmp)  # object names inside the .rda

    # Heuristic: prefer common model object names; else take the first
    pick <- NULL
    preferred <- c("fit", "model", "mod", "gbmFit", "glmnet.fit")
    for (cand in preferred) if (cand %in% objs) { pick <- cand; break }
    if (is.null(pick)) pick <- objs[[1L]]

    canonical <- tools::file_path_sans_ext(basename(p$file))  # e.g., rb_mod1a
    fits[[canonical]] <- get(pick, envir = tmp, inherits = FALSE)
  }
  fits
}


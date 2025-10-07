# tests/testthat/test-artifact-registry-registry.R

.quiet_eval <- function(expr) {
  withCallingHandlers(
    suppressWarnings(suppressMessages(force(expr))),
    message  = function(m) invokeRestart("muffleMessage"),
    warning  = function(w) invokeRestart("muffleWarning")
  )
}

testthat::test_that(".wa_registry prefers configured CSV; errors when missing/columns missing", {
  wa_registry <- getFromNamespace(".wa_registry", "writeAlizer")

  # A) Valid temporary CSV with the required 6 columns
  good <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      kind = c("rds","rda"),
      model = c("m","m"),
      part = c("a","a"),
      file = c("m_vars.rds","m_a.rda"),
      url  = c("file:///tmp/v.rds","file:///tmp/a.rda"),
      sha  = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    ),
    good, row.names = FALSE
  )
  withr::local_options(writeAlizer.registry_csv = good)
  df <- wa_registry()
  testthat::expect_true(all(c("kind","model","part","file","url","sha") %in% names(df)))

  # B) Missing CSV → classed error
  withr::local_options(writeAlizer.registry_csv = tempfile()) # nonexistent
  testthat::expect_error(wa_registry(), class = "writeAlizer_registry_missing")

  # C) Missing required columns → classed error (malformed)
  bad <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(kind = "rda", model = "x", part = "a", file = "x.rda", stringsAsFactors = FALSE),
    bad, row.names = FALSE
  )
  withr::local_options(writeAlizer.registry_csv = bad)
  testthat::expect_error(wa_registry(), class = "writeAlizer_registry_malformed")
})

testthat::test_that(".wa_parts_for validates inputs; filters & orders parts; errors on bad registry", {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")

  # Input validation (vector 'kind' not allowed)
  testthat::expect_error(
    parts_for(kind = c("rds","rda"), model = "m"),
    "single string", ignore.case = TRUE
  )

  # Minimal good registry to exercise ordering by 'part'
  reg <- data.frame(
    kind  = c("rda","rda"),
    model = c("m","m"),
    part  = c("b","a"),
    file  = c("b.rda","a.rda"),
    url   = c("file:///b","file:///a"),
    sha   = NA_character_,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() reg)
  out <- parts_for("rda", "m")
  testthat::expect_equal(out$part, c("a","b"))

  # Broken registry (missing url/sha) → error
  bad_reg <- reg[c("kind","model","part","file")]
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() bad_reg)
  testthat::expect_error(parts_for("rda", "m"), "required", ignore.case = TRUE)
})

testthat::test_that("varlist & model loaders use registry and behave as implemented (consistent with .wa_load_varlists)", {
  load_varlists   <- getFromNamespace(".wa_load_varlists",   "writeAlizer")
  load_model_rdas <- getFromNamespace(".wa_load_model_rdas", "writeAlizer")

  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(
    writeAlizer.offline  = TRUE,
    download.file.method = "libcurl"
  )

  # Local artifacts
  r_exp  <- withr::local_tempfile(fileext = ".rds"); saveRDS(c("F01","F02"), r_exp)
  r_narr <- withr::local_tempfile(fileext = ".rds"); saveRDS(c("F03","F04"), r_narr)
  r_per  <- withr::local_tempfile(fileext = ".rds"); saveRDS(c("F05","F06"), r_per)

  f_a <- withr::local_tempfile(fileext = ".rda"); fit <- 11; save(fit, file = f_a)
  f_b <- withr::local_tempfile(fileext = ".rda"); fit <- 22; save(fit, file = f_b)
  f_c <- withr::local_tempfile(fileext = ".rda"); fit <- 33; save(fit, file = f_c)

  # IMPORTANT: sha must be "" (empty), not NA_character_
  varlist_df <- data.frame(
    kind  = c("rds","rds","rds"),
    model = c("rb_mod3all_v2","rb_mod3all_v2","rb_mod3all_v2"),
    part  = c("a","b","c"),
    file  = c("rb_exp_vars.rds","rb_narr_vars.rds","rb_per_vars.rds"),  # must end with *_vars.rds
    url   = c(
      paste0("file:///", normalizePath(r_exp,  winslash = "/")),
      paste0("file:///", normalizePath(r_narr, winslash = "/")),
      paste0("file:///", normalizePath(r_per,  winslash = "/"))
    ),
    sha   = c("", "", ""),    # <-- NOT NA
    stringsAsFactors = FALSE
  )
  withr::local_options(writeAlizer.artifacts_df = varlist_df)

  # .rda registry used by .wa_parts_for/.wa_load_model_rdas
  rda_df <- data.frame(
    kind  = c("rda","rda","rda"),
    model = c("rb_mod3all_v2","rb_mod3all_v2","rb_mod3all_v2"),
    part  = c("a","b","c"),
    file  = c("rb_mod3exp.rda","rb_mod3narr.rda","rb_mod3per.rda"),
    url   = c(
      paste0("file:///", normalizePath(f_a, winslash = "/")),
      paste0("file:///", normalizePath(f_b, winslash = "/")),
      paste0("file:///", normalizePath(f_c, winslash = "/"))
    ),
    sha   = c("", "", ""),    # <-- NOT NA
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() rda_df)

  # Varlists (names are full filenames, including .rds)
  v <- .quiet_eval(load_varlists("rb_mod3all_v2"))
  testthat::expect_true(is.list(v) && length(v) == 3L)
  testthat::expect_setequal(names(v), c("rb_exp_vars.rds","rb_narr_vars.rds","rb_per_vars.rds"))
  testthat::expect_identical(v[["rb_exp_vars.rds"]],  readRDS(r_exp))
  testthat::expect_identical(v[["rb_narr_vars.rds"]], readRDS(r_narr))
  testthat::expect_identical(v[["rb_per_vars.rds"]],  readRDS(r_per))

  # Model RDAs (last loaded wins = part 'c' -> fit == 33)
  env <- new.env(parent = emptyenv())
  .quiet_eval(load_model_rdas("rb_mod3all_v2", envir = env))
  testthat::expect_true(exists("fit", envir = env))
  testthat::expect_identical(get("fit", envir = env), 33)
})

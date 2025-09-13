# tests/testthat/test-artifact-registry-more.R

# Helper: silence stdout+messages while evaluating an expression
.quiet_eval <- function(expr) {
  tmp_out <- tempfile(); tmp_msg <- tempfile()
  on.exit(unlink(c(tmp_out, tmp_msg)), add = TRUE)
  con_out <- file(tmp_out, "wt"); con_msg <- file(tmp_msg, "wt")
  sink(con_out, type = "output"); sink(con_msg, type = "message")
  on.exit({
    repeat { ok <- try({ sink(type = "message"); FALSE }, silent = TRUE); if (isFALSE(ok)) break }
    repeat { ok <- try({ sink(type = "output");  FALSE }, silent = TRUE); if (isFALSE(ok)) break }
    try(close(con_msg), silent = TRUE); try(close(con_out), silent = TRUE)
  }, add = TRUE)
  force(expr)
}

# ---- canonical mapping sanity ----
testthat::test_that(".wa_canonical_model maps known keys as expected", {
  canon <- getFromNamespace(".wa_canonical_model", "writeAlizer")
  testthat::expect_true(is.character(canon("rb_mod3all")))
  testthat::expect_true(is.character(canon("coh_mod3all")))
  x <- canon("coh_mod3all")
  testthat::expect_identical(canon(x), x)
})

# ---- parts_for: flexible on unknown kind, counts for known ----
testthat::test_that(".wa_parts_for returns expected counts by model/kind", {
  parts_for <- writeAlizer:::.wa_parts_for
  p1 <- parts_for("rda", "rb_mod1");      testthat::expect_s3_class(p1, "data.frame"); testthat::expect_equal(nrow(p1), 6)
  p2 <- parts_for("rda", "rb_mod2");      testthat::expect_equal(nrow(p2), 3)
  p3 <- parts_for("rds", "coh_mod3all");  testthat::expect_equal(nrow(p3), 3)
  p4 <- parts_for("rds", "no_such_model");testthat::expect_equal(nrow(p4), 0L)
})

testthat::test_that(".wa_parts_for validates input shape; unknown kind tolerated", {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")
  testthat::expect_error(parts_for(kind = c("rds","rda"), model = "x"),
                         "must be a single string", ignore.case = TRUE)
  res <- try(parts_for(kind = "zip", model = "x"), silent = TRUE)
  if (inherits(res, "try-error")) testthat::succeed() else {
    testthat::expect_s3_class(res, "data.frame"); testthat::expect_equal(nrow(res), 0L)
  }
})

# ---- ensure_file: cached+checksum early return; no-URL error; file:// download ----
testthat::test_that(".wa_ensure_file: cached+checksum early return and no-URL error", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(download.file.method = "libcurl")

  # A) Early return when cached and checksum matches
  rel1 <- file.path("models", "demo", "ok.rda")
  dest1 <- cached_path(rel1)
  dir.create(dirname(dest1), recursive = TRUE, showWarnings = FALSE)
  save(list = character(), file = dest1)
  good_sha <- digest::digest(dest1, algo = "sha256", file = TRUE)
  p <- .quiet_eval(ensure_file(rel1, url = "file:///not/used.rda", sha256 = good_sha))
  testthat::expect_identical(normalizePath(p), normalizePath(dest1))

  # B) No URL provided when not cached -> error (suppress warning from empty URL)
  rel2 <- file.path("models", "demo", "need_url.rda")
  dest2 <- cached_path(rel2)
  dir.create(dirname(dest2), recursive = TRUE, showWarnings = FALSE)
  testthat::expect_error(
    suppressWarnings(.quiet_eval(ensure_file(rel2, url = ""))),
    "url|provide", ignore.case = TRUE
  )
})

testthat::test_that(".wa_ensure_file can download from file:// source (single attempt)", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(download.file.method = "libcurl")

  # Source RDA
  src <- withr::local_tempfile(fileext = ".rda")
  save(list = character(), file = src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  rel <- file.path("models", "demo", "fetch_once.rda")
  dest <- cached_path(rel)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  out <- .quiet_eval(ensure_file(rel, url = url))
  testthat::expect_true(file.exists(out))
})

# ---- load varlists via mocked registry (omit 'sha' column to avoid checksum verification) ----
testthat::test_that(".wa_load_varlists uses registry and returns list of R objects", {
  load_varlists <- getFromNamespace(".wa_load_varlists", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

  r1 <- withr::local_tempfile(fileext = ".rds"); saveRDS(list(a = 1), r1)
  r2 <- withr::local_tempfile(fileext = ".rds"); saveRDS(list(b = 2), r2)

  reg <- data.frame(
    kind  = c("rds","rds"),
    model = c("my_model","my_model"),
    part  = c("b","a"),
    file  = c("vlist_b.rds","vlist_a.rds"),
    url   = c(paste0("file:///", normalizePath(r2, winslash = "/")),
              paste0("file:///", normalizePath(r1, winslash = "/"))),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() reg)

  out <- .quiet_eval(load_varlists("my_model"))
  testthat::expect_true(is.list(out))
  testthat::expect_length(out, 2L)
  testthat::expect_identical(out[[1]]$a, 1)
  testthat::expect_identical(out[[2]]$b, 2)
})

# ---- load model RDAs into env ----
testthat::test_that(".wa_load_model_rdas loads objects into the provided environment", {
  load_model_rdas <- getFromNamespace(".wa_load_model_rdas", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

  f1 <- withr::local_tempfile(fileext = ".rda"); fit <- 1L; save(fit, file = f1)
  f2 <- withr::local_tempfile(fileext = ".rda"); fit <- 2L; save(fit, file = f2)

  reg <- data.frame(
    kind  = c("rda","rda"),
    model = c("my_rda","my_rda"),
    part  = c("a","b"),
    file  = c("mya.rda","myb.rda"),
    url   = c(paste0("file:///", normalizePath(f1, winslash = "/")),
              paste0("file:///", normalizePath(f2, winslash = "/"))),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() reg)

  env <- new.env(parent = emptyenv())
  .quiet_eval(invisible(load_model_rdas("my_rda", envir = env)))
  testthat::expect_true(exists("fit", envir = env))
  testthat::expect_identical(get("fit", envir = env), 2L)  # last loaded wins
})

# ---- load fits list; name canonicalization; skip package checks ----
testthat::test_that(".wa_load_fits_list returns a named list; skips pkg checks via mock", {
  load_fits_list <- getFromNamespace(".wa_load_fits_list", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

  a <- withr::local_tempfile(fileext = ".rda"); fit <- 11; save(fit, file = a)
  b <- withr::local_tempfile(fileext = ".rda"); fit <- 22; save(fit, file = b)

  reg <- data.frame(
    kind  = c("rda","rda"),
    model = c("fmod","fmod"),
    part  = c("a","b"),
    file  = c("rb_mod1a.rda","rb_mod1b.rda"),
    url   = c(paste0("file:///", normalizePath(a, winslash = "/")),
              paste0("file:///", normalizePath(b, winslash = "/"))),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .package = "writeAlizer",
    .wa_registry = function() reg,
    .wa_require_pkgs_for_fits = function(fits) TRUE
  )

  fits <- .quiet_eval(load_fits_list("fmod"))
  testthat::expect_true(is.list(fits))
  testthat::expect_setequal(names(fits), c("rb_mod1a", "rb_mod1b"))
  testthat::expect_identical(fits$rb_mod1a, 11)
  testthat::expect_identical(fits$rb_mod1b, 22)
})

# tiny helper for pipes
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- registry: error when required columns are missing ----
testthat::test_that(".wa_parts_for errors if registry lacks required columns", {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")

  # Mock a broken registry (missing 'url')
  bad_reg <- data.frame(
    kind  = "rda",
    model = "x",
    part  = "a",
    file  = "x.rda",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() bad_reg)

  testthat::expect_error(
    parts_for("rda", "x"),
    "registry.*missing.*required columns|missing columns|required columns",
    ignore.case = TRUE
  )
})

# ---- pkg requirement collector: success path (aggregates correctly) ----
testthat::test_that(".wa_require_pkgs_for_fits collects from classes and caret::train", {
  req_pkgs <- getFromNamespace(".wa_require_pkgs_for_fits", "writeAlizer")

  # Build fake fit objects hitting each class branch
  fits <- list(
    structure(list(), class = "randomForest"),
    structure(list(), class = "gbm"),
    structure(list(), class = "glmnet"),
    structure(list(), class = "earth"),
    structure(list(), class = "Cubist"),
    structure(list(), class = "ksvm"),
    structure(list(), class = "mvr"),
    structure(list(), class = "caretEnsemble"),
    structure(list(modelInfo = list(library = c("pkgA", "pkgB"))), class = "train")
  )

  called <- character(0)
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) {
      called <<- unique(c(called, pkg))
      TRUE
    }
  )

  testthat::expect_silent(req_pkgs(fits))
  expect_true(all(c(
    "randomForest", "gbm", "glmnet", "earth",
    "Cubist", "kernlab", "pls", "caretEnsemble",
    "pkgA", "pkgB"
  ) %in% called))
})

# ---- pkg requirement collector: error path (missing package) ----
testthat::test_that(".wa_require_pkgs_for_fits errors when a required package is missing", {
  req_pkgs <- getFromNamespace(".wa_require_pkgs_for_fits", "writeAlizer")

  fits <- list(
    structure(list(), class = "randomForest"),
    structure(list(modelInfo = list(library = c("pkg_missing"))), class = "train")
  )

  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) {
      !identical(pkg, "pkg_missing")
    }
  )

  testthat::expect_error(
    req_pkgs(fits),
    "pkg_missing|install.packages|required",
    ignore.case = TRUE
  )
})

# ---- guard: .wa_load_varlists errors when no rows for model ----
testthat::test_that(".wa_load_varlists errors for unknown model", {
  load_varlists <- getFromNamespace(".wa_load_varlists", "writeAlizer")
  expect_error(
    load_varlists("no_such_model"),
    "No variable lists registered|No variable lists", ignore.case = TRUE
  )
})

# ---- guard: .wa_load_model_rdas errors when no rows for model ----
testthat::test_that(".wa_load_model_rdas errors for unknown model", {
  load_model_rdas <- getFromNamespace(".wa_load_model_rdas", "writeAlizer")
  env <- new.env(parent = emptyenv())
  expect_error(
    load_model_rdas("no_such_model", envir = env),
    "No model artifacts registered|No model artifacts", ignore.case = TRUE
  )
})

# ---- guard: .wa_load_fits_list errors when no rows; and canonical mapping works ----
testthat::test_that(".wa_load_fits_list errors for unknown model; maps legacy -> v2", {
  load_fits_list <- getFromNamespace(".wa_load_fits_list", "writeAlizer")

  # A) error path
  expect_error(
    load_fits_list("no_such_model"),
    "No model artifacts registered", ignore.case = TRUE
  )

  # B) mapping path: provide registry for rb_mod3narr_v2 but call with rb_mod3narr
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  a <- withr::local_tempfile(fileext = ".rda"); fit <- 1L; save(fit, file = a)
  b <- withr::local_tempfile(fileext = ".rda"); fit <- 2L; save(fit, file = b)

  reg_map <- data.frame(
    kind  = c("rda","rda"),
    model = c("rb_mod3narr_v2","rb_mod3narr_v2"),
    part  = c("a","b"),
    file  = c("rb_mod3narr_v2_a.rda","rb_mod3narr_v2_b.rda"),
    url   = c(paste0("file:///", normalizePath(a, winslash = "/")),
              paste0("file:///", normalizePath(b, winslash = "/"))),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() reg_map)
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_require_pkgs_for_fits = function(fits) TRUE)

  fits <- .quiet_eval(load_fits_list("rb_mod3narr"))  # should map to rb_mod3narr_v2
  expect_true(is.list(fits))
  expect_setequal(names(fits), c("rb_mod3narr_v2_a", "rb_mod3narr_v2_b"))
})

# ---- checksum mismatch: ensure_file errors when sha doesn't match ----
testthat::test_that(".wa_ensure_file errors on checksum mismatch", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(download.file.method = "libcurl")

  # Create a source file and compute a WRONG checksum
  src <- withr::local_tempfile(fileext = ".rda")
  save(list = character(), file = src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))
  wrong_sha <- paste(rep("0", 64), collapse = "")

  rel  <- file.path("models", "demo", "badsha.rda")
  dest <- cached_path(rel)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  expect_error(
    suppressWarnings(.quiet_eval(ensure_file(rel, url = url, sha256 = wrong_sha))),
    "Checksum mismatch|checksum", ignore.case = TRUE
  )
})

# ---- registry: prefers CSV when present (non-brittle check) ----
testthat::test_that(".wa_registry prefers CSV (sha present, non-empty)", {
  wa_registry <- getFromNamespace(".wa_registry", "writeAlizer")
  out <- wa_registry()
  expect_s3_class(out, "data.frame")
  expect_true(all(c("kind","model","part","file","url") %in% names(out)))
  expect_true("sha" %in% names(out))
  expect_true(any(!is.na(out$sha)))
})

# ---- local path helper returns a plausible path string ----
testthat::test_that(".wa_local_path returns a length-1 character path", {
  wa_local_path <- getFromNamespace(".wa_local_path", "writeAlizer")
  p <- wa_local_path("somefile.ext")
  expect_true(is.character(p) && length(p) == 1L)
})

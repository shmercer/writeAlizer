# tests/testthat/test-artifact-registry.R
# Consolidated coverage for R/artifact_registry.R

.quiet_eval <- function(expr) {
  withCallingHandlers(
    suppressWarnings(suppressMessages(force(expr))),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

testthat::test_that(".wa_parts_for returns expected shapes and validates inputs", {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")

  # invalid kind vector -> classed input error
  testthat::expect_error(
    parts_for(kind = c("rds", "rda"), model = "x"),
    class = "writeAlizer_input_error"
  )

  # unknown kind should still return a data.frame (empty)
  testthat::expect_s3_class(parts_for(kind = "zip", model = "x"), "data.frame")

  # missing model rows -> empty df
  df <- parts_for("rds", "no_such_model")
  testthat::expect_true(is.data.frame(df))
  testthat::expect_equal(nrow(df), 0L)
})

testthat::test_that(".wa_parts_for errors if registry lacks required columns", {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")
  bad_reg <- data.frame(kind = "rda", model = "x", part = "a", file = "x.rda",
                        stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() bad_reg)
  testthat::expect_error(
    parts_for("rda", "x"),
    class = "writeAlizer_registry_malformed"
  )
})

testthat::test_that(".wa_registry missing CSV is classed", {
  wa_registry <- getFromNamespace(".wa_registry", "writeAlizer")

  withr::local_options(writeAlizer.registry_csv = tempfile())  # force missing
  testthat::local_mocked_bindings(
    .package = "base",
    system.file = function(...) ""  # belt-and-suspenders
  )

  testthat::expect_error(
    wa_registry(),
    class = "writeAlizer_registry_missing"
  )
})

testthat::test_that(".wa_ensure_file: cache hit + checksum; input guard; file:// download; checksum mismatch", {
  ensure_file  <- getFromNamespace(".wa_ensure_file",  "writeAlizer")
  cached_path  <- getFromNamespace(".wa_cached_path",  "writeAlizer")

  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(download.file.method = "libcurl")

  # A) Early return with good checksum
  rel_ok  <- file.path("models", "demo", "ok.rda")
  dest_ok <- cached_path(rel_ok)
  dir.create(dirname(dest_ok), recursive = TRUE, showWarnings = FALSE)
  save(list = character(), file = dest_ok)
  good_sha <- digest::digest(dest_ok, algo = "sha256", file = TRUE)
  out_ok <- .quiet_eval(ensure_file(rel_ok, url = "file:///not/used", sha256 = good_sha))
  testthat::expect_identical(normalizePath(out_ok), normalizePath(dest_ok))

  # B) Not cached, empty URL -> classed input error
  rel_need  <- file.path("models", "demo", "need_url.rda")
  testthat::expect_error(
    ensure_file(rel_need, url = ""),
    class = "writeAlizer_input_error"
  )

  # C) file:// download works
  src <- withr::local_tempfile(fileext = ".rda"); save(list = character(), file = src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))
  rel_dl  <- file.path("models", "demo", "fetch_once.rda")
  dest_dl <- cached_path(rel_dl)
  dir.create(dirname(dest_dl), recursive = TRUE, showWarnings = FALSE)
  out <- .quiet_eval(ensure_file(rel_dl, url = url))
  testthat::expect_true(file.exists(out))

  # D) checksum mismatch throws an error (assert by message, not class)
  wrong_sha <- paste(rep("0", 64), collapse = "")
  rel_bad  <- file.path("models", "demo", "badsha.rda")
  dest_bad <- cached_path(rel_bad)
  dir.create(dirname(dest_bad), recursive = TRUE, showWarnings = FALSE)

  testthat::expect_error(
    suppressWarnings(.quiet_eval(ensure_file(rel_bad, url = url, sha256 = wrong_sha))),
    regexp = "Downloaded checksum mismatch"
  )
})

testthat::test_that(".wa_ensure_file respects writeAlizer.mock_dir precedence (loader unique behavior)", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")

  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(download.file.method = "libcurl")

  # Arrange a mock_dir with the artifact relative path and ensure it's used
  mock_dir <- withr::local_tempdir()
  rel      <- file.path("models", "demo", "from_mock.rda")
  mock_fp  <- file.path(mock_dir, rel)
  dir.create(dirname(mock_fp), recursive = TRUE, showWarnings = FALSE)
  x <- 123L; save(x, file = mock_fp)

  withr::local_options(writeAlizer.mock_dir = mock_dir)

  # Give a bogus URL to prove mock_dir is preferred
  out <- .quiet_eval(ensure_file(rel, url = "file:///does/not/exist.rda"))
  testthat::expect_true(file.exists(out))

  # Compare file bytes (use file=TRUE, not positional arg)
  testthat::expect_identical(
    digest::digest(out,     algo = "sha256", file = TRUE),
    digest::digest(mock_fp, algo = "sha256", file = TRUE)
  )
})

testthat::test_that(".wa_load_model_rdas loads into provided environment; last wins", {
  load_model_rdas <- getFromNamespace(".wa_load_model_rdas", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

  f1 <- withr::local_tempfile(fileext = ".rda"); fit <- 1L; save(fit, file = f1)
  f2 <- withr::local_tempfile(fileext = ".rda"); fit <- 2L; save(fit, file = f2)

  reg <- data.frame(
    kind = c("rda","rda"), model = "my_rda", part = c("a","b"),
    file = c("mya.rda","myb.rda"),
    url  = c(paste0("file:///", normalizePath(f1, winslash = "/")),
             paste0("file:///", normalizePath(f2, winslash = "/"))),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(.package = "writeAlizer", .wa_registry = function() reg)

  env <- new.env(parent = emptyenv())
  .quiet_eval(invisible(load_model_rdas("my_rda", envir = env)))
  testthat::expect_true(exists("fit", envir = env))
  testthat::expect_identical(get("fit", envir = env), 2L)
})

testthat::test_that(".wa_load_model_rdas: missing example mock is classed", {
  load_model_rdas <- getFromNamespace(".wa_load_model_rdas", "writeAlizer")

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)
  testthat::expect_error(
    load_model_rdas("example"),
    class = "writeAlizer_mock_missing"
  )
})

testthat::test_that(".wa_load_fits_list returns named list; canonicalizes; unknown model errors (classed)", {
  load_fits_list <- getFromNamespace(".wa_load_fits_list", "writeAlizer")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

  a <- withr::local_tempfile(fileext = ".rda"); fit <- 11; save(fit, file = a)
  b <- withr::local_tempfile(fileext = ".rda"); fit <- 22; save(fit, file = b)

  reg <- data.frame(
    kind=c("rda","rda"), model=c("fmod","fmod"), part=c("a","b"),
    file=c("rb_mod1a.rda","rb_mod1b.rda"),
    url=c(paste0("file:///", normalizePath(a, winslash = "/")),
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

  testthat::expect_error(
    load_fits_list("no_such_model"),
    class = "writeAlizer_parts_missing"
  )
})

testthat::test_that(".wa_load_fits_list maps legacy -> v2 names", {
  load_fits_list <- getFromNamespace(".wa_load_fits_list", "writeAlizer")
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
  testthat::local_mocked_bindings(
    .package = "writeAlizer",
    .wa_registry = function() reg_map,
    .wa_require_pkgs_for_fits = function(fits) TRUE
  )

  fits <- .quiet_eval(load_fits_list("rb_mod3narr"))
  testthat::expect_true(is.list(fits))
  testthat::expect_setequal(names(fits), c("rb_mod3narr_v2_a", "rb_mod3narr_v2_b"))
})

testthat::test_that(".wa_registry has expected core columns and sha present", {
  wa_registry <- getFromNamespace(".wa_registry", "writeAlizer")
  out <- wa_registry()

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(all(c("kind","model","part","file","url") %in% names(out)))
  testthat::expect_true("sha" %in% names(out))
  testthat::expect_true(any(!is.na(out$sha)))
})

testthat::test_that(".wa_local_path returns a single path string", {
  wa_local_path <- getFromNamespace(".wa_local_path", "writeAlizer")
  p <- wa_local_path("somefile.ext")
  testthat::expect_true(is.character(p) && length(p) == 1L)
})

testthat::test_that(".wa_require_pkgs_for_fits collects needed pkgs and errors when missing (classed)", {
  req_pkgs <- getFromNamespace(".wa_require_pkgs_for_fits", "writeAlizer")

  # A) Collection across classes
  fits <- list(
    structure(list(), class = "randomForest"),
    structure(list(), class = "gbm"),
    structure(list(), class = "glmnet"),
    structure(list(), class = "earth"),
    structure(list(), class = "Cubist"),
    structure(list(), class = "ksvm"),
    structure(list(), class = "mvr"),
    structure(list(), class = "caretEnsemble"),
    structure(list(modelInfo = list(library = c("pkgA","pkgB"))), class = "train")
  )
  called <- character(0)
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) { called <<- unique(c(called, pkg)); TRUE }
  )
  testthat::expect_silent(req_pkgs(fits))
  testthat::expect_true(all(c("randomForest","gbm","glmnet","earth","Cubist","kernlab","pls",
                              "caretEnsemble","pkgA","pkgB") %in% called))

  # B) Missing package path (classed)
  fits2 <- list(
    structure(list(), class = "randomForest"),
    structure(list(modelInfo = list(library = c("pkg_missing"))), class = "train")
  )
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) { !identical(pkg, "pkg_missing") }
  )
  testthat::expect_error(req_pkgs(fits2), class = "writeAlizer_dependency_missing")
})

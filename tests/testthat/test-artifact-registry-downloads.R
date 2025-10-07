# tests/testthat/test-artifact-registry-downloads.R

# Helper: silence stdout+messages/warnings while evaluating an expression
.quiet_eval <- function(expr) {
  withCallingHandlers(
    suppressWarnings(suppressMessages(force(expr))),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

testthat::test_that(".wa_ensure_file validates inputs and supports file:// downloads with checksum paths", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")

  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(download.file.method = "libcurl")

  # A) Input validation (non-empty character scalars)
  testthat::expect_error(
    ensure_file(NA, url = "x"),
    class = "writeAlizer_input_error"
  )
  testthat::expect_error(
    ensure_file("models/x.rda", url = ""),
    class = "writeAlizer_input_error"
  )

  # B) Happy path: download from file://, then cache hit (same path)
  src <- withr::local_tempfile(fileext = ".rda")
  fit <- 1L; save(fit, file = src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  rel  <- file.path("models", "demo", "tiny_fit.rda")
  dest <- cached_path(rel)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  p1 <- .quiet_eval(ensure_file(rel, url = url))
  testthat::expect_true(file.exists(p1))
  testthat::expect_identical(normalizePath(p1), normalizePath(dest))

  # C) Cached + correct checksum → early return (no re-download)
  good_sha <- digest::digest(dest, algo = "sha256", file = TRUE)
  p2 <- .quiet_eval(ensure_file(rel, url = "file:///not/used", sha256 = good_sha))
  testthat::expect_identical(normalizePath(p2), normalizePath(dest))
})

testthat::test_that(".wa_ensure_file emits mismatch warning, then errors on post-download mismatch; respects offline", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")
  cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")

  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(download.file.method = "libcurl")

  # Prepare a tiny source
  src <- withr::local_tempfile(fileext = ".rda"); save(list = character(), file = src)
  url <- paste0("file:///", normalizePath(src, winslash = "/"))

  # 1) Put a corrupted cached copy to trigger "cached mismatch → warn + redownload"
  rel   <- file.path("models", "demo", "mismatch.rda")
  dest  <- cached_path(rel)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(1:8), dest) # corrupt cached file
  correct_sha <- digest::digest(src, algo = "sha256", file = TRUE)

  got_warn <- FALSE
  withCallingHandlers(
    {
      # IMPORTANT: call ensure_file() directly (no .quiet_eval) and quiet = FALSE
      ensure_file(rel, url = url, sha256 = correct_sha, quiet = FALSE)
    },
    warning = function(w) {
      if (grepl("Checksum mismatch", conditionMessage(w), ignore.case = TRUE)) {
        got_warn <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  testthat::expect_true(got_warn)

  # 2) Now force a *post-download* mismatch to hit the classed error path
  bad_sha <- paste(rep("0", 64), collapse = "")
  suppressWarnings(
    testthat::expect_error(
      .quiet_eval(ensure_file(rel, url = url, sha256 = bad_sha, quiet = TRUE)),
      class = "writeAlizer_checksum_mismatch"
    )
  )

  # 3) Offline guard for non-file URLs (classed)
  withr::local_options(writeAlizer.offline = TRUE)
  testthat::expect_error(
    ensure_file("models/x/y.bin", url = "https://example.invalid/x.bin"),
    class = "writeAlizer_offline"
  )
})

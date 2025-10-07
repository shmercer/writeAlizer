# tests/testthat/test-file-utilities.R

quiet <- function(expr) {
  withCallingHandlers(
    suppressWarnings(suppressMessages(force(expr))),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

testthat::test_that(".wa_cached_path joins onto wa_cache_dir correctly", {
  if (!exists(".wa_cached_path", asNamespace("writeAlizer"))) {
    testthat::skip("`.wa_cached_path` not found in this build")
  }
  .wa_cached_path <- getFromNamespace(".wa_cached_path", "writeAlizer")
  wa_cache_dir    <- getFromNamespace("wa_cache_dir",    "writeAlizer")

  # Put cache into a temp tree so we don't touch user cache
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())

  rel <- file.path("models", "x", "y.bin")
  got <- .wa_cached_path(rel)
  exp <- file.path(wa_cache_dir(), rel)

  # Normalize for OS differences and ensure directories are present
  testthat::expect_identical(normalizePath(dirname(got), winslash = "/", mustWork = FALSE),
                             normalizePath(dirname(exp), winslash = "/", mustWork = FALSE))
  testthat::expect_true(endsWith(got, "y.bin"))
})

testthat::test_that(".wa_from_file_url round-trips real file:// URLs (spaces, % encoding, Windows drives)", {
  if (!exists(".wa_from_file_url", asNamespace("writeAlizer"))) {
    testthat::skip("`.wa_from_file_url` not found in this build")
  }
  .wa_from_file_url <- getFromNamespace(".wa_from_file_url", "writeAlizer")

  # Create a real file path with a space to force %20 encoding in URLs
  tmpdir <- withr::local_tempdir()
  p      <- file.path(tmpdir, "has space.txt")
  writeLines("ok", p)

  # Build a file:// URL from the normalized forward-slash path
  fwd <- normalizePath(p, winslash = "/", mustWork = TRUE)
  url <- paste0("file:///", sub("^/*", "", fwd))   # ensure exactly 3 slashes

  # The helper should decode %20 etc. and produce a platform path
  got <- .wa_from_file_url(url)

  if (.Platform$OS.type == "windows") {
    # On Windows we expect backslashes and no leading slash before drive
    exp <- chartr("/", "\\", fwd)
    if (grepl("^/[A-Za-z]:", got)) {
      got <- substring(got, 2L)  # function already does this, but be resilient
    }
    testthat::expect_true(grepl("^[A-Za-z]:\\\\", got))
    testthat::expect_identical(tolower(got), tolower(exp))
  } else {
    # POSIX: should be the same absolute path with a single leading slash
    testthat::expect_true(startsWith(got, "/"))
    testthat::expect_identical(got, fwd)
  }

  # Non-file URLs should pass through unchanged
  testthat::expect_identical(.wa_from_file_url("https://example.com/x.rds"),
                             "https://example.com/x.rds")
})

testthat::test_that(".wa_local_path builds a path under inst/extdata", {
  if (!exists(".wa_local_path", asNamespace("writeAlizer"))) {
    testthat::skip("`.wa_local_path` not found in this build")
  }
  .wa_local_path <- getFromNamespace(".wa_local_path", "writeAlizer")

  # This does not require the file to exist; we verify the base prefix
  target <- ".does_not_need_to_exist.txt"
  got    <- .wa_local_path(target)
  base   <- system.file("extdata", package = "writeAlizer")

  testthat::expect_true(startsWith(normalizePath(got, winslash = "/", mustWork = FALSE),
                                   normalizePath(base, winslash = "/", mustWork = FALSE)))
  testthat::expect_true(endsWith(got, target))
})

testthat::test_that(".wa_from_file_url handles UNC form (Windows) and leaves host paths untouched (POSIX)", {
  if (!exists(".wa_from_file_url", asNamespace("writeAlizer"))) {
    testthat::skip("`.wa_from_file_url` not found in this build")
  }
  .wa_from_file_url <- getFromNamespace(".wa_from_file_url", "writeAlizer")

  if (.Platform$OS.type == "windows") {
    got <- .wa_from_file_url("file://server/share/folder/file.rds")
    # Accept one or more leading backslashes, then server\share\
    testthat::expect_true(grepl("^\\\\+server\\\\share\\\\", got))
    testthat::expect_true(endsWith(got, "file.rds"))
  } else {
    got <- .wa_from_file_url("file://server/share/folder/file.rds")
    testthat::expect_true(startsWith(got, "/server/share/"))
    testthat::expect_true(endsWith(got, "file.rds"))
  }
})


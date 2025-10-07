# tests/testthat/test-artifact-registry-paths.R

# Helper: silence stdout+messages/warnings while evaluating an expression
.quiet_eval <- function(expr) {
  withCallingHandlers(
    suppressWarnings(suppressMessages(force(expr))),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning")
  )
}

testthat::test_that(".wa_from_file_url normalizes file:// URLs and returns non-file unchanged", {
  from_file_url <- getFromNamespace(".wa_from_file_url", "writeAlizer")

  # Non-file: returned as-is
  testthat::expect_identical(from_file_url("https://example.com/x"), "https://example.com/x")

  # POSIX-style normalization: collapse extra slashes and decode %20
  tmp <- withr::local_tempfile()
  writeLines("x", tmp)
  raw   <- paste0("file:////", utils::URLencode(normalizePath(tmp, winslash = "/")))
  out   <- from_file_url(raw)
  # on POSIX we keep a single leading slash; on Windows we'll get backslashes later
  testthat::expect_true(grepl("writeLines", readLines(out, warn = FALSE)[1L]) || file.exists(out))

  # Windows-specific branches are OS-dependent; just verify it doesn’t error.
  testthat::expect_silent(from_file_url(paste0("file:///", utils::URLencode(normalizePath(tmp, winslash = "/")))))
})

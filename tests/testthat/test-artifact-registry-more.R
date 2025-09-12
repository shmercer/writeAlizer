# tests/testthat/test-artifact-registry-more.R

testthat::test_that(".wa_canonical_model maps known keys as expected", {
  canon <- getFromNamespace(".wa_canonical_model", "writeAlizer")
  # Use a few stable examples; add/adjust if your registry evolves
  testthat::expect_true(is.character(canon("rb_mod3all")))
  testthat::expect_true(is.character(canon("coh_mod3all")))
  # idempotent on canonical names
  x <- canon("coh_mod3all")
  testthat::expect_identical(canon(x), x)
})

testthat::test_that(".wa_parts_for returns data with a filename-like column and handles bad type", {
  parts_for <- getFromNamespace(".wa_parts_for", "writeAlizer")

  # A valid request should return a data.frame (possibly empty on some builds)
  p1 <- parts_for("rds", "rb_mod3all")
  testthat::expect_s3_class(p1, "data.frame")
  any_file_col <- any(vapply(p1, function(col)
    is.character(col) && any(grepl("\\.(rds|rda)$", col, ignore.case = TRUE), na.rm = TRUE),
    logical(1)))
  testthat::expect_true(any_file_col || nrow(p1) == 0)

  # Unknown artifact type: allow either an error OR an empty data.frame
  got_err <- FALSE
  res <- NULL
  tryCatch({ res <- parts_for("zip", "rb_mod3all") }, error = function(e) got_err <<- TRUE)

  if (!got_err) {
    testthat::expect_s3_class(res, "data.frame")
    testthat::expect_equal(nrow(res), 0L)
  } else {
    testthat::succeed()
  }
})

testthat::test_that(".wa_ensure_file returns mock-dir file and errors when missing", {
  ensure_file <- getFromNamespace(".wa_ensure_file", "writeAlizer")

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  # success: create a nested path and ensure_file should resolve to it
  nested <- file.path("subdir", "mock_obj.rda")
  full   <- file.path(tmp, nested)
  dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
  save(list = character(), file = full)

  out <- ensure_file(nested, url = "https://example.invalid/does-not-matter")
  testthat::expect_identical(normalizePath(out), normalizePath(full))

  # failure: ask for a file that doesn't exist in mock_dir (no network fallback here)
  testthat::expect_error(
    ensure_file("missing_here.rda", url = "file:///definitely-not-there.rda"),
    "not found|missing|ensure", ignore.case = TRUE
  )
})

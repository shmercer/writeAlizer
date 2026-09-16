test_that("local URLs preserve absolute paths, decode spaces, and reuse verified cache", {
  src <- file.path(withr::local_tempdir(), "file with space.bin")
  writeBin(as.raw(1:5), src)
  withr::local_options(writeAlizer.cache_dir = withr::local_tempdir(),
                       writeAlizer.mock_dir = NULL, writeAlizer.offline = TRUE)
  url <- to_file_url(src)
  sha <- digest::digest(src, algo = "sha256", file = TRUE)
  dest <- wa_download("nested/artifact.bin", url, toupper(sha))
  expect_identical(readBin(dest, "raw", 5), as.raw(1:5))
  expect_identical(wa_download("nested/artifact.bin", "https://unused.invalid", sha), dest)
  expect_true(file.exists(wa_download("no-sha.bin", url, NA_character_)))
  expect_true(file.exists(wa_download("logical-na-sha.bin", url, NA)))
  expect_true(file.exists(wa_download("empty-sha.bin", url, "")))
})

test_that("file URL parsing handles Windows drives and network shares", {
  expect_identical(.wa_from_file_url("FILE:///C:/a%20b/x.rds", windows = TRUE), "C:\\a b\\x.rds")
  expect_identical(.wa_from_file_url("file://C:/x", windows = TRUE), "C:\\x")
  expect_identical(.wa_from_file_url("file://server/share/x", windows = TRUE), "\\\\server\\share\\x")
  expect_identical(.wa_from_file_url("file:////server/share/x", windows = TRUE), "\\\\server\\share\\x")
  expect_identical(.wa_from_file_url("file://localhost/tmp/x", windows = FALSE), "/tmp/x")
  expect_identical(.wa_from_file_url("file:///tmp/x", windows = FALSE), "/tmp/x")
})

test_that("artifact input validation runs before filesystem access", {
  for (file in list(NA_character_, character(), c("a", "b"), "", "../x", "/tmp/x", "C:/x", "a/../x", "a\\..\\x")) {
    expect_error(wa_download(file, "file:///unused"), class = "writeAlizer_input_error")
  }
  for (url in list(NA_character_, character(), c("a", "b"), "")) {
    expect_error(wa_download("x", url), class = "writeAlizer_input_error")
  }
  for (sha in list(1, character(), c("a", "b"), "xyz")) {
    expect_error(wa_download("x", "file:///unused", sha), class = "writeAlizer_input_error")
  }
  for (tries in list(NA_real_, Inf, -1, 0.5, .Machine$integer.max, c(1, 2))) {
    expect_error(.wa_ensure_file("x", "file:///unused", max_retries = tries), class = "writeAlizer_input_error")
  }
  expect_error(wa_download("x", "file:///unused", quiet = NA), class = "writeAlizer_input_error")
  withr::local_options(writeAlizer.mock_dir = withr::local_tempdir())
  expect_error(wa_download("absent", "file:///unused"), class = "writeAlizer_mock_missing")
})

test_that("remote transfers retry failures and publish only verified data", {
  cache <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = cache, writeAlizer.mock_dir = NULL)
  calls <- 0L
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    calls <<- calls + 1L
    if (calls == 1L) return(1L)
    writeLines("verified", destfile)
    0L
  }, .package = "utils")
  expect_message(path <- .wa_ensure_file("ok.bin", "https://unused.invalid"), "Downloaded model artifact")
  expect_identical(calls, 2L)
  expect_identical(readLines(path), "verified")
  expect_identical(list.files(cache, all.files = TRUE, no.. = TRUE), "ok.bin")
})

test_that("checksum failures preserve an existing cache and clean staging files", {
  cache <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = cache, writeAlizer.mock_dir = NULL)
  writeLines("old", file.path(cache, "x"))
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("unverified", destfile)
    0L
  }, .package = "utils")
  expect_error(wa_download("x", "https://unused.invalid", strrep("0", 64)), class = "writeAlizer_checksum_mismatch")
  expect_identical(readLines(file.path(cache, "x")), "old")
  expect_identical(list.files(cache, all.files = TRUE, no.. = TRUE), "x")
  expect_error(wa_download("new", "https://unused.invalid", strrep("0", 64)), class = "writeAlizer_checksum_mismatch")
  expect_false(file.exists(file.path(cache, "new")))
})

test_that("failed copy, move, or cache creation is reported instead of succeeding", {
  cache <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = cache, writeAlizer.mock_dir = NULL)
  src <- withr::local_tempfile()
  writeLines("source", src)
  url <- to_file_url(src)
  with_mocked_bindings(
    expect_error(.wa_ensure_file("x", url, max_retries = 0), class = "writeAlizer_download_failed"),
    file.copy = function(...) FALSE, .package = "base"
  )
  with_mocked_bindings(
    expect_error(.wa_ensure_file("x", url, max_retries = 0), class = "writeAlizer_download_failed"),
    file.rename = function(...) FALSE, .package = "base"
  )
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)
  dir.create(file.path(cache, "directory"))
  expect_error(wa_download("directory", url), class = "writeAlizer_download_failed")
  writeLines("file blocks directory", file.path(cache, "block"))
  expect_error(wa_download("block/x", url), class = "writeAlizer_download_failed")
})

test_that("registry rejects missing values and duplicate model parts", {
  reg <- .wa_registry()[1L, ]
  path <- withr::local_tempfile(fileext = ".csv")
  withr::local_options(writeAlizer.registry_csv = path)
  for (bad in list(transform(reg, url = NA_character_), transform(reg, kind = "zip"), rbind(reg, reg))) {
    write.csv(bad, path, row.names = FALSE)
    expect_error(.wa_registry(), class = "writeAlizer_registry_malformed")
  }
})

test_that("both loaders honor checksums and reject empty fit archives", {
  src <- withr::local_tempfile(fileext = ".rda")
  save(list = character(), file = src)
  reg <- data.frame(kind = "rda", model = "test", part = "a", file = "test.rda",
                    url = to_file_url(src), sha = strrep("0", 64))
  withr::local_options(writeAlizer.cache_dir = withr::local_tempdir(), writeAlizer.mock_dir = NULL)
  local_mocked_bindings(.wa_registry = function() reg)
  expect_error(suppressWarnings(.wa_load_model_rdas("test", new.env())), class = "writeAlizer_checksum_mismatch")
  expect_error(suppressWarnings(.wa_load_fits_list("test")), class = "writeAlizer_checksum_mismatch")
  reg$sha <- NA_character_
  expect_error(suppressMessages(.wa_load_fits_list("test")), "contains no objects", class = "writeAlizer_artifact_missing")
})

test_that("example archives and nested caret dependencies are checked", {
  dir <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = dir)
  expect_error(.wa_load_fits_list("example"), class = "writeAlizer_mock_missing")
  save(list = character(), file = file.path(dir, "example.rda"))
  expect_error(.wa_load_fits_list("example"), "contains no objects")
  model <- stats::lm(y ~ 1, data.frame(y = 1:2))
  save(model, file = file.path(dir, "example.rda"))
  expect_s3_class(.wa_load_fits_list("example")$example, "lm")
  env <- new.env()
  expect_invisible(.wa_load_model_rdas("example", env))
  expect_s3_class(env$model, "lm")
  fit <- structure(list(modelInfo = list(library = "missingNestedModelPackage")), class = "train")
  ensemble <- structure(list(models = list(fit)), class = "caretEnsemble")
  expect_error(.wa_require_pkgs_for_fits(list(ensemble)), "missingNestedModelPackage",
               class = "writeAlizer_dependency_missing")
})

test_that("legacy variable-list keys resolve v2 files through the configured registry", {
  src <- withr::local_tempfile(fileext = ".rds")
  saveRDS(c("feature_a", "feature_b"), src)
  csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(kind = "rds", model = "rb_mod3narr_v2", part = "a",
                       file = "rb_narr_vars_v2.rds", url = to_file_url(src),
                       sha = digest::digest(src, algo = "sha256", file = TRUE)), csv, row.names = FALSE)
  withr::local_options(writeAlizer.registry_csv = csv, writeAlizer.artifacts_df = NULL,
                       writeAlizer.mock_dir = NULL, writeAlizer.cache_dir = withr::local_tempdir())
  lists <- .wa_load_varlists("rb_mod3narr")
  expect_identical(lists, list(rb_narr_vars_v2.rds = c("feature_a", "feature_b")))
  reg <- read.csv(csv)
  reg$sha <- NA
  write.csv(reg, csv, row.names = FALSE)
  expect_identical(.wa_load_varlists("rb_mod3narr"), lists)
})

test_that("all CSV importers preserve numeric-looking identifiers", {
  fixtures <- list(
    coh = list(header = "TextID,x", rows = c("002,2", "001,1"), read = import_coh),
    rb = list(header = "File.name,x", rows = c("002,2", "001,1"), read = import_rb),
    gamet = list(header = "filename,error_count,word_count,grammar,misspelling,duplication,typographical,whitespace",
                 rows = c("002,0,10,0,0,0,0,0", "001,0,20,0,0,0,0,0"), read = import_gamet)
  )
  for (f in fixtures) {
    path <- withr::local_tempfile(fileext = ".csv")
    writeLines(c(f$header, f$rows), path)
    expect_identical(f$read(path)$ID, c("001", "002"))
  }
})

test_that("missing, blank, duplicate and ambiguous identifiers fail clearly", {
  for (ids in list(c(NA, "a"), c(" ", "a"), c("a", "a"))) {
    expect_error(.wa_validate_import(data.frame(ID = ids), "ID"), class = "writeAlizer_input_error")
  }
  expect_identical(.wa_validate_import(list(ID = c("a", "b")), "ID")$ID, c("a", "b"))
  expect_error(.wa_validate_import(new.env(), "ID"), class = "writeAlizer_input_error")
  expect_error(.wa_validate_import(data.frame(x = 1), "x"), "ID", class = "writeAlizer_input_error")
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("TextID,ID,x", "a,b,1"), path)
  expect_error(import_coh(path), "both", class = "writeAlizer_input_error")
  writeLines(c("File.name,ID", "a,b"), path)
  expect_error(import_rb(path), "both", class = "writeAlizer_input_error")
  writeLines(c("wrong,x", "a,1"), path)
  expect_error(import_gamet(path), "filename", class = "writeAlizer_input_error")
  expect_error(import_coh(path), "TextID", class = "writeAlizer_input_error")
  expect_error(import_rb(path), "ID", class = "writeAlizer_input_error")
})

test_that("GAMET rejects nonnumeric measurements before computing rates", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("filename,error_count,word_count,grammar,misspelling,duplication,typographical,whitespace",
               "a,0,ten,0,0,0,0,0"), path)
  expect_error(import_gamet(path), "numbers", class = "writeAlizer_input_error")
  expect_true(.wa_num_like(c("1.", ".5", "-1.e2", NA)))
  expect_false(.wa_num_like(c("1", "not a number")))
})

test_that("path options reject malformed values and empty options use defaults", {
  withr::local_options(writeAlizer.cache_dir = NULL)
  default <- wa_cache_dir()
  withr::local_options(writeAlizer.cache_dir = "")
  expect_identical(wa_cache_dir(), default)
  for (value in list(NA_character_, character(), c("a", "b"), 1)) {
    withr::local_options(writeAlizer.cache_dir = value)
    expect_error(wa_cache_dir(), class = "writeAlizer_input_error")
  }
  for (arg in list(NA, c(TRUE, FALSE), "yes")) {
    expect_error(wa_cache_clear(ask = arg), class = "writeAlizer_input_error")
    expect_error(wa_cache_clear(preview = arg), class = "writeAlizer_input_error")
  }
})

test_that("cache previews handle empty and unavailable file sizes", {
  cache <- withr::local_tempdir()
  withr::local_options(writeAlizer.cache_dir = cache, writeAlizer.force_interactive = TRUE,
                       writeAlizer.menu_fn = function(...) 1L)
  expect_message(expect_false(wa_cache_clear(ask = TRUE)), "0 B")
  original_info <- base::file.info
  with_mocked_bindings(
    expect_message(expect_false(wa_cache_clear(ask = TRUE)), "unknown"),
    file.info = function(...) {
      if (!length(list(...)[[1]])) stop("cannot stat")
      original_info(...)
    }, .package = "base"
  )
  withr::local_options(writeAlizer.force_interactive = NULL)
  expect_identical(.wa_is_interactive(), interactive())
})

test_that("dependency reporting handles absent Suggests and empty overrides", {
  suggests <- NA_character_
  local_mocked_bindings(packageDescription = function(...) suggests, .package = "utils")
  withr::local_options(writeAlizer.required_pkgs = NULL)
  expect_message(out <- model_deps(), "No optional")
  expect_identical(out, list(required = character(), missing = character()))
  suggests <- "stats,\n utils"
  withr::local_options(writeAlizer.required_pkgs = c(NA_character_, "", "  stats  "))
  expect_message(out <- model_deps(), "All required")
  expect_setequal(out$required, c("stats", "utils"))
  expect_length(out$missing, 0L)
})

test_that("ReaderBench sample discovery handles unavailable or empty headers", {
  with_mocked_bindings(expect_null(.wa_rb_keep_exclude_from_sample()),
                       file.exists = function(...) FALSE, .package = "base")
  with_mocked_bindings(expect_null(.wa_rb_keep_exclude_from_sample()),
                       read.csv = function(...) data.frame(), .package = "utils")
})


test_that("example seeding validates its directory and reports creation failures", {
  for (dir in list(NA_character_, character(), c("a", "b"), "")) {
    expect_error(wa_seed_example_models(dir = dir), class = "writeAlizer_input_error")
  }
  path <- withr::local_tempfile()
  writeLines("not a directory", path)
  expect_error(wa_seed_example_models(dir = path), "Cannot create", class = "writeAlizer_input_error")
})


test_that("blank measurements stay numeric and duplicate CSV headers are rejected", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("TextID,x", "001,1", "002,", "003,   "), path)
  out <- import_coh(path)
  expect_identical(out$ID, c("001", "002", "003"))
  expect_identical(out$x, c(1, NA_real_, NA_real_))
  for (header in c("TextID,x,x", "File.name,x,x", "filename,word_count,word_count")) {
    writeLines(c(header, "a,1,2"), path)
    fn <- if (startsWith(header, "TextID")) import_coh else if (startsWith(header, "File")) import_rb else import_gamet
    expect_error(fn(path), class = "writeAlizer_input_error")
  }
  writeLines(c("File.name,Score", "essay#1,3"), path)
  expect_identical(import_rb(path)$ID, "essay#1")
})

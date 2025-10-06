test_that(".wa_validate_import enforces ID presence, type, and uniqueness", {
  skip_on_cran()

  df <- data.frame(ID = c("a", "b", "c"), x = 1:3, stringsAsFactors = FALSE)
  expect_silent(.wa_validate_import(df, required = "ID", context = "x"))

  # ID missing
  expect_error(.wa_validate_import(df[ , "x", drop = FALSE], required = "ID"),
               class = "writeAlizer_input_error")

  # Duplicate IDs
  dup <- data.frame(ID = c("a","a"), x = 1:2)
  expect_error(.wa_validate_import(dup, required = "ID"),
               class = "writeAlizer_input_error")

  # ID coerced to character
  int_id <- data.frame(ID = 1:2, x = 1:2)
  out <- .wa_validate_import(int_id, required = "ID")
  expect_true(is.character(out$ID))
})

test_that("extra columns are allowed (no strict mode)", {
  df <- data.frame(ID = c("a","b"), known = 1:2, extra = 9:10, stringsAsFactors = FALSE)
  out <- writeAlizer:::.wa_validate_import(df, required = c("ID","known"), context = "demo")
  expect_true(all(c("ID","known","extra") %in% names(out)))
})


test_that("import_rb() integrates validator (ID )", {
  skip_on_cran()

  # Minimal RB-like CSV with header that import_rb keeps "by name"
  td <- withr::local_tempdir()
  csv <- file.path(td, "rb.csv")
  write.csv(data.frame(File.name = c("x","y"),
                       Score = 1:2,
                       stringsAsFactors = FALSE),
            csv, row.names = FALSE)

  out <- import_rb(csv)
  expect_true("ID" %in% names(out))
  expect_identical(out$ID, c("x","y"))

  # Duplicate IDs should trigger error (after normalization)
  csv2 <- file.path(td, "rb_dup.csv")
  write.csv(data.frame(File.name = c("x","x"), Score = 1:2), csv2, row.names = FALSE)
  expect_error(import_rb(csv2), class = "writeAlizer_input_error")
})

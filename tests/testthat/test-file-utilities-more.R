# tests/testthat/test-file-utilities-more.R

testthat::test_that("import_gamet: ID normalization, NaN→NA, numeric-like conversion, zero-division guard", {
  # Build a tiny GAMET-like CSV
  tmp <- withr::local_tempfile(fileext = ".csv")
  gam <- data.frame(
    filename      = c("C:/any/path/sub/file001.csv", "/whatever/dir/file002.csv"),
    error_count   = c("2", "NaN"),            # numeric-like + "NaN"
    word_count    = c(100, 0),                # second row 0 to hit NA guard
    grammar       = c("3", "5"),
    misspelling   = c("1", "2"),
    duplication   = c("0", "1"),
    typographical = c("0", "1"),
    whitespace    = c("1", "0"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(gam, tmp, row.names = FALSE)

  out <- writeAlizer::import_gamet(tmp)

  # Columns retained + derived
  expect_true(all(c("ID","error_count","word_count","grammar","misspelling",
                    "duplication","typographical","whitespace","per_gram","per_spell") %in% names(out)))

  # ID is basename sans extension (character)
  expect_identical(out$ID, c("file001", "file002"))

  # "NaN" turned into NA_real_ (after numeric-like conversion)
  expect_true(is.na(out$error_count[2]))
  expect_true(is.numeric(out$error_count))
  expect_true(is.numeric(out$grammar))

  # per_* computed; guarded when word_count == 0
  expect_equal(out$per_gram[1], 3/100)
  expect_equal(out$per_spell[1], 1/100)
  expect_true(is.na(out$per_gram[2]))
  expect_true(is.na(out$per_spell[2]))
})

testthat::test_that("import_coh: TextID→ID, numeric-like coercion, stable sort", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  coh <- data.frame(
    TextID    = c("z2", "a1"),           # out-of-order to verify sorting
    SomeVar   = c("10", "NaN"),          # numeric-like + "NaN"
    OtherVar  = c("1.5e1", "2.0"),       # exponent form
    stringsAsFactors = FALSE
  )
  utils::write.csv(coh, tmp, row.names = FALSE)

  out <- writeAlizer::import_coh(tmp)

  # ID created and sorting applied (character order "a1","z2")
  expect_identical(out$ID, c("a1", "z2"))

  # numeric-like columns coerced; "NaN" -> NA_real_
  expect_true(is.numeric(out$SomeVar))
  expect_true(is.na(out$SomeVar[1]))    # after sort, "NaN" row is first
  expect_equal(out$OtherVar, c(2.0, 15.0))
})

testthat::test_that("import_merge_gamet_rb: merges on ID as character and preserves per_*", {
  # Create GAMET file
  gm <- data.frame(
    filename      = c("1.csv", "2.csv"),
    error_count   = c(0, 1),
    word_count    = c(10, 10),
    grammar       = c(1, 2),
    misspelling   = c(0, 1),
    duplication   = c(0, 0),
    typographical = c(0, 0),
    whitespace    = c(0, 0),
    stringsAsFactors = FALSE
  )
  tmp_gm <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(gm, tmp_gm, row.names = FALSE)

  # Mock import_rb() to avoid CSV format brittleness but still exercise merge logic
  testthat::local_mocked_bindings(
    .package = "writeAlizer",
    import_rb = function(path) {
      data.frame(
        ID = c("1", "2"),
        F2 = c(0.1, 0.2),
        F3 = c(0.3, 0.4),
        stringsAsFactors = FALSE
      )
    }
  )

  # Dummy RB path (not used by our mock); function under test will call mocked import_rb()
  tmp_rb <- "dummy_rb.csv"

  merged <- writeAlizer::import_merge_gamet_rb(tmp_rb, tmp_gm)

  # Merged on ID; per_* present from GAMET import; and ID stays character and ordered
  expect_true(all(c("ID","per_gram","per_spell","F2","F3") %in% names(merged)))
  expect_identical(merged$ID, c("1","2"))
  expect_true(is.character(merged$ID))
  expect_equal(merged$per_gram, c(1/10, 2/10))
  expect_equal(merged$per_spell, c(0/10, 1/10))
})

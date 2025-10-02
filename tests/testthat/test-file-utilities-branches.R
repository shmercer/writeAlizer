# tests/testthat/test-file-utilities-branches.R

testthat::test_that(".wa_convert_numeric_like converts numeric-like strings (excluding columns)", {
  df <- data.frame(
    ID = c("a", "b"),
    num_chr = c("1.0", "2.5"),
    keep_chr = c("x", "y"),
    stringsAsFactors = FALSE
  )

  out <- writeAlizer:::.wa_convert_numeric_like(df, exclude = c("ID", "keep_chr"))
  # ID untouched
  testthat::expect_identical(out$ID, df$ID)
  # num_chr converted to numeric (branch inside the if(length(to_num)){})
  testthat::expect_type(out$num_chr, "double")
  testthat::expect_identical(out$num_chr, c(1.0, 2.5))
  # keep_chr stays character
  testthat::expect_type(out$keep_chr, "character")
})

testthat::test_that(".wa_naify_nan_chars replaces literal 'NaN' only in character cols", {
  df <- data.frame(
    ID = 1:3,
    ch = c("ok", "NaN", "fine"),
    num_chr = c("3", "4", "5"),  # remains character here
    stringsAsFactors = FALSE
  )
  out <- writeAlizer:::.wa_naify_nan_chars(df, exclude = "num_chr")
  testthat::expect_true(is.na(out$ch[2]))
  testthat::expect_identical(out$ch[c(1,3)], c("ok", "fine"))
  # excluded column unchanged
  testthat::expect_identical(out$num_chr, df$num_chr)
})

testthat::test_that("import_rb(): SEP header + read.table(text=readLines()) path + 'NaN' handling + ID normalization", {
  tmp <- withr::local_tempdir()

  # Provide a deterministic keep list via mocking so we don't skip if the sample header isn't present.
  testthat::with_mocked_bindings(
    .package = "writeAlizer",
    .wa_rb_keep_exclude_from_sample = function() {
      # Keep File.name so ID normalization path is exercised, plus 3 feature columns.
      list(keep = c("File.name", "RB.Keep1", "RB.Keep2", "RB.Keep3"),
           exclude = character(0))
    },
    {
      # Build a tiny 2-row CSV with SEP header forcing readLines() branch
      dat <- data.frame(
        check.names = FALSE,
        "File name" = c("docA", "docB"),
        stringsAsFactors = FALSE
      )
      # RB.Keep1: character with "NaN"/"text" to trigger .wa_naify_nan_chars()
      dat[["RB.Keep1"]] <- c("NaN", "text")
      # RB.Keep2 / RB.Keep3: numeric-like strings to trigger .wa_convert_numeric_like()
      dat[["RB.Keep2"]] <- c("1", "2")
      dat[["RB.Keep3"]] <- c("3", "4")

      header <- paste(names(dat), collapse = ",")
      rows   <- apply(dat, 1, function(r) paste(r, collapse = ","))
      csv    <- file.path(tmp, "rb_small_sep.csv")
      writeLines(c("SEP=,", header, rows), csv)

      rb <- writeAlizer::import_rb(csv)

      # 1) ID comes from "File name" (renamed to ID)
      testthat::expect_true("ID" %in% names(rb))
      testthat::expect_identical(rb$ID, c("docA", "docB"))

      # 2) 'NaN' -> NA for character columns
      testthat::expect_true(is.na(rb[["RB.Keep1"]][1]))
      testthat::expect_identical(rb[["RB.Keep1"]][2], "text")

      # 3) The chosen keep names made it through (exclude 'File.name' which is renamed to ID)
      testthat::expect_true(all(c("RB.Keep1","RB.Keep2","RB.Keep3") %in% names(rb)))
      # 4) Numeric-like keep cols converted
      testthat::expect_true(is.numeric(rb[["RB.Keep2"]]))
      testthat::expect_true(is.numeric(rb[["RB.Keep3"]]))
    }
  )
})

testthat::test_that("import_rb(): fallback branch when sample header helper returns NULL", {
  tmp <- withr::local_tempdir()

  # Minimal CSV that *doesn't* rely on the sample header logic
  dat <- data.frame(
    "File name" = c("doc1", "doc2"),
    RB.SomeMetric = c("1", "2"),
    Other = c("NaN", "ok"),
    stringsAsFactors = FALSE
  )
  csv <- file.path(tmp, "rb_fallback.csv")
  utils::write.table(dat, csv, sep = ",", row.names = FALSE, qmethod = "double")

  # Force the helper to return NULL so code hits the fallback:
  testthat::with_mocked_bindings(
    .package = "writeAlizer",
    .wa_rb_keep_exclude_from_sample = function() NULL,  # <- fallback trigger
    {
      rb <- writeAlizer::import_rb(csv)
      # Fallback still normalizes ID
      testthat::expect_true("ID" %in% names(rb))
      testthat::expect_identical(rb$ID, c("doc1", "doc2"))
      # And still NaN->NA on character columns
      testthat::expect_true(is.na(rb$Other[1]))
      testthat::expect_identical(rb$Other[2], "ok")
    })
})

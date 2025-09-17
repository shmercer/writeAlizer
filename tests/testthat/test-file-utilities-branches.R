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

  # Names to keep from the packaged sample header
  k <- writeAlizer:::.wa_rb_keep_exclude_from_sample()
  testthat::skip_if(is.null(k), "Sample RB header not available to build names")
  testthat::skip_if(length(k$keep) < 4, "Not enough names to exercise keep phase")

  # IMPORTANT: do NOT include 'File.name' in the trio we mutate
  keep_extras <- setdiff(k$keep, "File.name")
  testthat::skip_if(length(keep_extras) < 3, "Need at least 3 non-File.name keep names")
  keep3 <- keep_extras[seq_len(3)]

  # Build a tiny 2-row CSV:
  # - "File name" column (will become File.name -> ID)
  # - keep3[1]: character with "NaN"/"text" to trigger .wa_naify_nan_chars()
  # - keep3[2], keep3[3]: numeric-like strings
  dat <- data.frame(
    check.names = FALSE,
    "File name" = c("docA", "docB"),
    stringsAsFactors = FALSE
  )
  dat[[keep3[1]]] <- c("NaN", "text")
  dat[[keep3[2]]] <- c("1", "2")
  dat[[keep3[3]]] <- c("3", "4")

  # Force the 'SEP=,' branch (read.table(text = readLines(...)))
  header <- paste(names(dat), collapse = ",")
  rows   <- apply(dat, 1, function(r) paste(r, collapse = ","))
  csv    <- file.path(tmp, "rb_small_sep.csv")
  writeLines(c("SEP=,", header, rows), csv)

  rb <- writeAlizer::import_rb(csv)

  # 1) ID comes from "File name" (renamed to ID)
  testthat::expect_true("ID" %in% names(rb))
  testthat::expect_identical(rb$ID, c("docA", "docB"))

  # 2) 'NaN' -> NA for character columns
  testthat::expect_true(is.na(rb[[keep3[1]]][1]))
  testthat::expect_identical(rb[[keep3[1]]][2], "text")

  # 3) The chosen keep names made it through (exclude 'File.name' which is renamed to ID)
  testthat::expect_true(all(keep3 %in% names(rb)))
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

test_that("file URL helper round-trips paths with spaces", {
  path <- file.path(withr::local_tempdir(), "two words.txt")
  writeLines("example", path)
  expect_identical(normalizePath(.wa_from_file_url(to_file_url(path))), normalizePath(path))
})

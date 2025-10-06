test_that("offline error message is helpful (snapshot)", {
  skip_on_cran()
  cache_dir <- tools::R_user_dir("writeAlizer", "cache")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  withr::local_options(writeAlizer.offline = TRUE, writeAlizer.mock_dir = NULL)

  expect_snapshot(error = TRUE, {
    .wa_ensure_file("models/x/y.bin", "file:///nowhere.bin")
  })
})

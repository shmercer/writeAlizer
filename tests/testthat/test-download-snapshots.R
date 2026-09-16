test_that("offline error message is helpful (snapshot)", {
  skip_on_cran()
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  withr::local_options(writeAlizer.offline = TRUE, writeAlizer.mock_dir = NULL)

  expect_snapshot(error = TRUE, {
    .wa_ensure_file("models/x/y.bin", "file:///nowhere.bin")
  })
})

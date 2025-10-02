testthat::test_that("predict_quality() catches swapped args with helpful guidance", {
  dat <- data.frame(ID = 1:3, x = 1:3)

  # Pass a data.frame as the first (model) arg AND also provide `data` so the guard runs
  testthat::expect_error(
    writeAlizer::predict_quality(dat, data = dat),
    regexp = "function signature is predict_quality\\(model, data\\)|passed `data` as the first argument",
    class  = "writeAlizer_input_error"
  )
})

testthat::test_that("predict_quality() validates `model` and `data` types/columns", {
  dat <- data.frame(ID = 1:3, x = 1:3)

  expect_error(
    writeAlizer::predict_quality(NA_character_, dat),
    "`model` must be a non-empty character scalar",
    class = "writeAlizer_input_error"
  )

  expect_error(
    writeAlizer::predict_quality("rb_mod3all", as.list(dat)),
    "`data` must be a data.frame",
    class = "writeAlizer_input_error"
  )

  expect_error(
    writeAlizer::predict_quality("rb_mod3all", data.frame(x = 1:3)),
    "`data` must include an `ID` column",
    class = "writeAlizer_input_error"
  )
})

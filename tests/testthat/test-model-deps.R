test_that("model_deps() reports structure and emits a helpful message", {
  expect_message(md <- model_deps(),
                 regexp = "No optional model dependencies discovered|All required packages are installed|Missing required packages",
                 all = FALSE)
  expect_true(is.list(md))
  expect_named(md, c("required", "missing"))
  expect_true(is.character(md$required))
  expect_true(is.character(md$missing))
})

test_that("model_deps() prints an install command when something is missing (conditional)", {
  md <- model_deps()
  if (length(md$missing) == 0L) {
    testthat::skip("All optional deps are installed here; cannot exercise missing-branch.")
  }
  expect_message(model_deps(), regexp = "Install them manually, e.g.:\\s+install.packages\\(", all = FALSE)
})

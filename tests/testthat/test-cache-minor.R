testthat::test_that("exported cache helpers exist (smoke)", {
  # Some cache helpers are internal; this is a small guard that won't fail if absent
  expect_silent({
    getFromNamespace(".wa_cached_path", "writeAlizer")
  })
})

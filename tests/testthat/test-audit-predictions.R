test_that("unknown model and invalid data fail before loading artifacts", {
  local_mocked_bindings(.wa_registry = function() stop("must not read registry"))
  for (fn in list(preprocess, predict_quality)) {
    expect_error(fn("unknown", data.frame(ID = "a")), class = "writeAlizer_model_unknown")
    for (model in list(NA_character_, character(), c("example", "rb_mod1"))) {
      expect_error(fn(model, data.frame(ID = "a")), class = "writeAlizer_input_error")
    }
    expect_error(fn("example", data.frame(ID = character())), class = "writeAlizer_input_error")
    expect_error(fn("example", data.frame(ID = c("a", "a"))), class = "writeAlizer_input_error")
  }
  expect_error(predict_quality("gamet_cws1", data.frame(ID = "a")), "word_count")
  expect_error(predict_quality("gamet_cws1", data.frame(ID = "a", word_count = factor("12"), misspelling = 1)), "numeric")
})

test_that("preprocessing checks varlists, input features, and part counts", {
  dir <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = dir)
  reg <- data.frame(kind = "rds", model = "coh_mod3narr", part = "a", file = "vars.rds", url = "unused", sha = "")
  local_mocked_bindings(.wa_registry = function() reg)
  dat <- data.frame(ID = c("a", "b", "c"), x = c(1, 2, 3))
  for (vars in list(character(), "ID", c("x", "x"), NA_character_, 1)) {
    saveRDS(vars, file.path(dir, "vars.rds"))
    expect_error(preprocess("coh_mod3narr", dat), class = "writeAlizer_registry_malformed")
  }
  saveRDS("absent", file.path(dir, "vars.rds"))
  expect_error(preprocess("coh_mod3narr", dat), "absent", class = "writeAlizer_input_error")
  saveRDS("x", file.path(dir, "vars.rds"))
  expect_error(preprocess("coh_mod3narr", transform(dat, x = letters[1:3])), "numeric")
  expect_equal(preprocess("coh_mod3narr", dat)[[1]]$x, c(-1, 0, 1))
  # Preserve batch-based centering: the same text receives a different scaled value
  # when the other members of its scoring batch change.
  expect_false(isTRUE(all.equal(preprocess("coh_mod3narr", dat[1:2, ])[[1]]$x[1], -1)))
  reg <- rbind(reg, transform(reg, part = "b"))
  expect_error(preprocess("coh_mod3narr", dat), "Expected 1 varlist")
  reg$model <- "coh_mod2"
  expect_error(preprocess("coh_mod2", dat), "Expected 3 varlists")
})

test_that("prediction shapes are normalized only when they contain one value per text", {
  dat <- data.frame(ID = c("a", "b"), x = c(1, 2))
  value <- c(10, 20)
  local_mocked_bindings(.wa_load_fits_list = function(...) list(example = "fake"),
                        predict = function(...) value)
  for (shape in list(c(10, 20), matrix(c(10, 20), ncol = 1), data.frame(score = c(10, 20)))) {
    value <- shape
    expect_identical(predict_quality("example", dat)$pred_example, c(10, 20))
  }
  for (shape in list(1, numeric(), matrix(1:4, 2), data.frame(a = 1:2, b = 3:4), list(1, 2), array(1:2, c(2, 1, 1)))) {
    value <- shape
    expect_error(predict_quality("example", dat), class = "writeAlizer_prediction_error")
  }
  local_mocked_bindings(preprocess = function(model, data) list(data[2:1, ]))
  expect_error(predict_quality("example", dat), class = "writeAlizer_internal_mismatch")
})

test_that("aggregate and GAMET score values retain their established meanings", {
  dat <- data.frame(ID = c("b", "a"), word_count = c(12, 20), misspelling = c(2, 1))
  local_mocked_bindings(.wa_load_fits_list = function(model) {
    if (model == "gamet_cws1") list(CWS_mod1a = 3, CIWS_mod1a = 5)
    else setNames(as.list(1:6), paste0("coh_mod1", letters[1:6]))
  }, predict = function(object, newdata) rep(object, nrow(newdata)))
  gam <- predict_quality("gamet_cws1", dat)
  expect_named(gam, c("ID", "pred_TWW_gamet", "pred_WSC_gamet", "pred_CWS_mod1a", "pred_CIWS_mod1a"))
  expect_identical(gam$ID, dat$ID)
  expect_equal(gam$pred_WSC_gamet, c(10, 19))
  expect_equal(gam$pred_TWW_gamet, c(12, 20))
  expect_equal(gam$pred_CWS_mod1a, c(3, 3))
  expect_equal(gam$pred_CIWS_mod1a, c(5, 5))
  expect_equal(predict_quality("coh_mod1", dat)$pred_coh_mod1_mean, c(3.5, 3.5))
})

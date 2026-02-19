test_that("code runs when F-tests are invalid by df1", {
  prep_remedi = prepare_data_frame(remedi) %>%
    dplyr::filter(group == "NCC")

  model_fit = dd_hyperbolic_model(prep_remedi)
  param_est = estimate_hyperbolic_model_params(prep_remedi)
  expect_equal(model_fit, param_est)
})


test_that("code runs when F-tests are invalid by df2", {
  prep_remedi = prepare_data_frame(remedi) %>%
    dplyr::filter(subj %in% c(10168449, 10458654))

  model_fit = dd_hyperbolic_model(prep_remedi)
  param_est = estimate_hyperbolic_model_params(prep_remedi)
  expect_equal(model_fit, param_est)
})

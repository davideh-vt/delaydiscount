test_that("get subj est lnk works on valid input", {
  # Testing this function in relation to prepare_data_frame because the output
  #  is basically expected to match similar calculations from the
  #  prepare_data_frame function.
  # Potential problems could include:
  #  The output not being ordered correctly
  #  The formula to calculate the parameter estimate being implemented incorrectly
  # Both can be checked against prepare_data_frame output, and if that is incorrect,
  #  then the prepare_data_frame tests should catch that.
  prep_dd_data = prepare_data_frame(remedi)
  est_ln_k_df = get_subj_est_ln_k(prep_dd_data)

  expect_equal(rep(est_ln_k_df$ln_k, each = 7), prep_dd_data$lin_ln_k)
})


test_that("error thrown on non-prepared data", {
  # This input check just checks that all the expected variables are present,
  #  since otherwise we'd have to redo the entire process of preparing the df
  expect_error(get_subj_est_ln_k(remedi),
               "The input data frame should be output from the prepare_data_frame function. Extra variables may be added to the output, but preexisting variables must not be deleted, modified, or renamed.")
})

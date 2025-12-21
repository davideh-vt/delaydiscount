test_that("prepare data frame works", {
  subj = c(rep(1, 5), rep(2, 5), rep(1, 5))
  group = c(rep("Baseline", 10), rep("EFT", 5))
  delay = 1:5
  indiff = c(0.75, 0.54, 0.75, 0.6, 0.64,
             0.75, 0.54, 0.75, 0.96, 0.64,
             0.99, 0.99, 0.99, 0.99, 0.90)

  dd_data = data.frame(subj, group, delay, indiff)
  prep_dd_data = prepare_data_frame(dd_data)
  # Problem: these tests are rather circular
  expected_log_delay = log(dd_data$delay)
  expected_transform_indiff = log(1/dd_data$indiff - 1)
  expect_equal(prep_dd_data$log_delay, expected_log_delay)
  expect_equal(prep_dd_data$indiff_transform, expected_transform_indiff)

  # Check that the data are ordered correctly
  expect_equal(dd_data$subj, prep_dd_data$subj)
  expect_equal(dd_data$group, prep_dd_data$group)
  expect_equal(dd_data$delay, prep_dd_data$delay)

  expected_est_ln_k = c(
    mean(expected_transform_indiff[1:5]) - mean(expected_log_delay[1:5]),
    mean(expected_transform_indiff[6:10]) - mean(expected_log_delay[6:10]),
    mean(expected_transform_indiff[11:15]) - mean(expected_log_delay[11:15])
  )
  expected_est_ln_k = rep(expected_est_ln_k, each = 5)
  expect_equal(expected_est_ln_k, prep_dd_data$lin_ln_k)

  # Check that permuting order of input does not affect the output
  dd_data = dd_data[15:1,]
  prep_dd_data_1 = prepare_data_frame(dd_data)
  expect_equal(prep_dd_data, prep_dd_data_1)
})

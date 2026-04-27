test_that("dataframe structure is correct", {
  dd_data <- simulate_dataset(conditions = c("EFT", "NCC"),
                              num_subj = c(75, 150),
                              time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                              mean_ln_k = c(-6.7, -6),
                              sigma_sq = 1.5, g = 10)
  expect_equal(dd_data$subj, rep(1:225, each = 7))
  expect_equal(dd_data$group, c(rep("EFT", 75*7), rep("NCC", 150*7)))
  expect_equal(dd_data$delay,
               rep(c(30, 90, 180, 365, 1095, 1825, 3650), 225))
  indiff_transform <- log(1/dd_data$indiff - 1)
  # Test compatibility of output df with our other methods
  expect_no_error(prepare_data_frame(dd_data))
  expect_no_error(jb_rule_check(dd_data))

})

test_that("simulation works as expected", {
  set.seed(1)
  dd_data <- simulate_dataset(conditions = c("EFT", "NCC"),
                              num_subj = c(75, 150),
                              time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                              mean_ln_k = c(-6.7, -6),
                              sigma_sq = 1.5, g = 10)
  indiff_transform <- log(1/dd_data$indiff - 1)
  set.seed(1)
  # Recreate ln_k for EFT
  eft_ln_k <- rnorm(75)*sqrt(10*1.5/7) - 6.7
  # Recreate variance components
  eft_rv <- rnorm(75*7)*sqrt(1.5)
  # Similar for NCC
  ncc_ln_k <- rnorm(150)*sqrt(10*1.5/7) - 6
  ncc_rv <- rnorm(150*7)*sqrt(1.5)
  # Recreate transformed indifference
  indiff_transform_2 <- rep(log(c(30, 90, 180, 365, 1095, 1825, 3650)), 225) +
    rep(c(eft_ln_k, ncc_ln_k), each = 7) +
    c(eft_rv, ncc_rv)
  expect_equal(indiff_transform, indiff_transform_2)
})

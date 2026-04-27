test_that("dataframe structure is correct", {
  dd_data <- simulate_dataset(conditions = c("EFT", "NCC"),
                              num_subj = c(75, 150),
                              time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                              mean_ln_k = c(-6.7, -6),
                              sigma_sq = 1.5, g = 10)
  # Test structure is as expected
  expect_equal(dd_data$subj, rep(1:225, each = 7))
  expect_equal(dd_data$group, c(rep("EFT", 75*7), rep("NCC", 150*7)))
  expect_equal(dd_data$delay,
               rep(c(30, 90, 180, 365, 1095, 1825, 3650), 225))
  # Test compatibility of output df with our other methods
  expect_no_error(prepare_data_frame(dd_data))
  expect_no_error(jb_rule_check(dd_data))
})

test_that("simulation works as expected", {
  set.seed(23)
  dd_data <- simulate_dataset(conditions = c("EFT", "NCC"),
                              num_subj = c(75, 150),
                              time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                              mean_ln_k = c(-6.7, -6),
                              sigma_sq = 1.5, g = 10)
  indiff_transform <- log(1/dd_data$indiff - 1)
  set.seed(23)
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
  expect_equal(rep(eft_ln_k, each = 7), dd_data$true_ln_k[1:(75*7)])
  expect_equal(rep(ncc_ln_k, each = 7), dd_data$true_ln_k[(75*7+1):(225*7)])
})

test_that("condition order need not be alphabetical", {
  set.seed(27)
  dd_data <- simulate_dataset(conditions = c("NCC", "EFT"),
                              num_subj = c(120, 80),
                              time_points = c(30, 90, 180, 365, 1095, 1825),
                              mean_ln_k = c(-5.4, -6.2),
                              sigma_sq = 2, g = 8)
  # Test structure is as expected
  expect_equal(dd_data$subj, rep(1:200, each = 6))
  expect_equal(dd_data$group, c(rep("NCC", 120*6), rep("EFT", 80*6)))
  expect_equal(dd_data$delay,
               rep(c(30, 90, 180, 365, 1095, 1825), 200))

  # Test simulation procedure is as expected
  indiff_transform <- log(1/dd_data$indiff - 1)

  set.seed(27)
  # Recreate ln_k for NCC
  ncc_ln_k <- rnorm(120)*sqrt(8*2/6) - 5.4
  # Recreate variance components
  ncc_rv <- rnorm(120*6)*sqrt(2)
  # Similar for EFT
  eft_ln_k <- rnorm(80)*sqrt(8*2/6) - 6.2
  eft_rv <- rnorm(80*6)*sqrt(2)
  # Recreate transformed indifference
  indiff_transform_2 <- rep(log(c(30, 90, 180, 365, 1095, 1825)), 200) +
    rep(c(ncc_ln_k, eft_ln_k), each = 6) +
    c(ncc_rv, eft_rv)
  expect_equal(indiff_transform, indiff_transform_2)
  expect_equal(rep(ncc_ln_k, each = 6), dd_data$true_ln_k[1:(120*6)])
  expect_equal(rep(eft_ln_k, each = 6), dd_data$true_ln_k[(120*6+1):(200*6)])
})

test_that("errors are given when appropriate", {
  expect_error(simulate_dataset(conditions = c("EFT", "EFT"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = 1.5, g = 10),
               "All conditions must be unique.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC", "HIT"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = 1.5, g = 10),
               "num_subj must have length equal to the number of conditions.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6, -6.21),
                                sigma_sq = 1.5, g = 10),
               "mean_ln_k must have length equal to the number of conditions.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(0, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = 1.5, g = 10),
               "Numbers of subjects must be positive.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(75, -150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = 1.5, g = 10),
               "Numbers of subjects must be positive.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = c(1, 2), g = 10),
               "Variance components sigma_sq and g must each be length 1.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = 1.5, g = 9:10),
               "Variance components sigma_sq and g must each be length 1.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = -1.5, g = 10),
               "Variance components sigma_sq and g must not be negative.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = 1.5, g = -10),
               "Variance components sigma_sq and g must not be negative.")

  expect_error(simulate_dataset(conditions = c("EFT", "NCC"),
                                num_subj = c(75, 150),
                                time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                                mean_ln_k = c(-6.7, -6),
                                sigma_sq = -1.5, g = -10),
               "Variance components sigma_sq and g must not be negative.")
})

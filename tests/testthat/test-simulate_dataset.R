test_that("dataframe structure is correct", {
  dd_data <- simulate_dataset(conditions = c("EFT", "NCC"),
                              num_subj = c(75, 150),
                              time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                              mean_ln_k = c(-6.7, -6),
                              sigma_sq = 1.5, g = 10)
  expect_equal(length(dd_data$))
})

test_that("insufficient columns throw error", {
  # Test that the correct error message is thrown when any of the columns
  #  subj, group, delay, or indiff are missing

  remedi_rm_var <- remedi %>%
    select(study, subj, group, indiff)
  expect_error(check_input_preconditions(remedi_rm_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

  remedi_rm_var <- remedi %>%
    select(study, subj, group, delay)
  expect_error(check_input_preconditions(remedi_rm_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

  remedi_rm_var <- remedi %>%
    select(study, subj, delay, indiff)
  expect_error(check_input_preconditions(remedi_rm_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

  remedi_rm_var <- remedi %>%
    select(study, group, delay, indiff)
  expect_error(check_input_preconditions(remedi_rm_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

  remedi_rm_var <- remedi %>%
    select(subj, group, delay)
  expect_error(check_input_preconditions(remedi_rm_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

  remedi_rm_var <- remedi %>%
    select(subj, delay)
  expect_error(check_input_preconditions(remedi_rm_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

  remedi_rm_var <- remedi %>%
    select(study)
  expect_error(check_input_preconditions(remedi_rm_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

  # Test that even if one of the required variable names is duplicated and the
  #  total of matching variable names is 4, the check still won't pass.
  remedi_dup_var <- remedi
  names(remedi_dup_var)[5] = "delay"
  # "study" "subj" "group" "delay" "delay"
  expect_error(check_input_preconditions(remedi_dup_var),
               "The input data frame must have columns for subj, group, delay, and indiff")

})



test_that("structure required", {
  # Test that errors related to invalid structure trigger when appropriate

  msg1 = "The input data frame must have no more than one observation per time point for each subject within group."
  msg2 = "Every subject within group must have an observation for each time point."
  warn_msg = "The models in this package assume discounting curves are independent. Subjects appearing in more than one group may violate this assumption."

  # Repeat group subj delay combinations should throw msg1
  remedi_mod_delay <- remedi
  remedi_mod_delay$delay[2] = remedi_mod_delay$delay[1]
  expect_error(check_input_preconditions(remedi_mod_delay),
               msg1)
  remedi_mod_delay$delay[1:7] = remedi_mod_delay$delay[1]
  expect_error(check_input_preconditions(remedi_mod_delay),
               msg1)

  # Repeat subj delay combos should not throw msg1 if group is not repeated
  #  but they should throw a warning
  remedi_mod_subj <- remedi
  remedi_mod_subj$subj[8:14] = remedi_mod_subj$subj[1:7]
  expect_no_error(suppressWarnings(check_input_preconditions(remedi_mod_subj)))
  expect_warning(check_input_preconditions(remedi_mod_subj), warn_msg)
  expect_no_warning(check_input_preconditions(remedi))

  remedi_mod_subj$group[8] = remedi_mod_subj$group[1]
  expect_error(check_input_preconditions(remedi_mod_subj),
               msg1)


  # Mismatch delays should throw message 2
  remedi_mod_delay <- remedi
  remedi_mod_delay$delay[1] = 20
  expect_error(check_input_preconditions(remedi_mod_delay),
               msg2)

  # Extra delays should throw message 2
  remedi_mod_delay <- rbind(remedi_mod_delay[1,], remedi)
  expect_error(check_input_preconditions(remedi_mod_delay),
               msg2)

  # Missing delays should throw message 2
  remedi_mod_delay <- remedi_mod_delay[c(-1,-2),]
  expect_error(check_input_preconditions(remedi_mod_delay),
               msg2)

  # Out of order observation should not throw error message
  remedi_mod_delay <- rbind(remedi_mod_delay, remedi[1,])
  expect_no_error(check_input_preconditions(remedi_mod_delay))

})



test_that("delays must be positive", {
  # Test that the correct error message is thrown when any of the columns
  #  subj, group, delay, or indiff are missing

  msg = "All delays must be positive."

  remedi_neg_delay <- remedi
  remedi_neg_delay$delay = -remedi_neg_delay$delay
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  # Zeros should also trigger this
  remedi_neg_delay <- remedi
  remedi_neg_delay$delay[which(remedi_neg_delay$delay == 30)] = 0
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  # Test on a small dataset
  remedi_neg_delay <- remedi_neg_delay[1:7,]
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  remedi_neg_delay$delay[1] = -1
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  remedi_neg_delay <- remedi_neg_delay[1:7,]
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  remedi_neg_delay = remedi_neg_delay[1:4,]
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)
})



test_that("delays must be positive", {
  # Test that the correct error message is thrown when any delays are nonpositive
  # (and a different error would not be thrown for a different issue)

  msg = "All delays must be positive."

  remedi_neg_delay <- remedi
  remedi_neg_delay$delay = -remedi_neg_delay$delay
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  # Zeros should also trigger this
  remedi_neg_delay <- remedi
  remedi_neg_delay$delay[which(remedi_neg_delay$delay == 30)] = 0
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  # Test on a small dataset
  remedi_neg_delay <- remedi_neg_delay[1:7,]
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  remedi_neg_delay$delay[1] = -1
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  remedi_neg_delay <- remedi_neg_delay[1:7,]
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)

  remedi_neg_delay = remedi_neg_delay[1:4,]
  expect_error(check_input_preconditions(remedi_neg_delay),
               msg)
})



test_that("indiff pts must be in range", {
  # Test that the correct error message is thrown when any indifference points
  #  are not in range

  msg = "All indifference points must be between 0 and 1, exclusive."

  remedi_mod_indiff <- remedi
  remedi_mod_indiff$indiff[1] = 0
  expect_error(check_input_preconditions(remedi_mod_indiff),
               msg)

  remedi_mod_indiff <- remedi
  remedi_mod_indiff$indiff[44] = 1
  expect_error(check_input_preconditions(remedi_mod_indiff),
               msg)


  remedi_mod_indiff <- remedi
  remedi_mod_indiff$indiff[127] = -0.5
  expect_error(check_input_preconditions(remedi_mod_indiff),
               msg)

  remedi_mod_indiff <- remedi
  remedi_mod_indiff$indiff[1999] = 1.2
  expect_error(check_input_preconditions(remedi_mod_indiff),
               msg)
})

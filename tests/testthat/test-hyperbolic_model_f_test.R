test_that("two level test", {
  # Calculate the p-value using two tests that are known to be equivalent, then
  #  show that they produce the same answer
  prep_remedi = prepare_data_frame(remedi)
  prep_remedi_rm_HIT = prep_remedi %>%
    dplyr::filter(group != "HIT")  # TODO: Why does filter not work without dplyr::?
  test_res = hyperbolic_model_f_test(prep_remedi_rm_HIT, list(c("EFT", "NCC")))
  est_ln_k_df = get_subj_est_ln_k(prep_remedi)
  t_test_method = t.test(est_ln_k_df$ln_k[est_ln_k_df$group == "EFT"],
                         est_ln_k_df$ln_k[est_ln_k_df$group == "NCC"],
                         var.equal = TRUE)

  expect_equal(test_res$p_value, t_test_method$p.value)
  expect_equal(test_res$F_stat, as.numeric(t_test_method$statistic^2))
  expect_equal(test_res$df1, 1)
  expect_equal(test_res$df2, as.numeric(t_test_method$parameter["df"]))
})


test_that("three level test", {
  # Now do three-way F-test
  prep_remedi = prepare_data_frame(remedi)
  test_res = hyperbolic_model_f_test(prep_remedi, list(c("EFT", "NCC", "HIT")))
  est_ln_k_df = get_subj_est_ln_k(prep_remedi)
  lm_method = summary(lm(ln_k~group, data = est_ln_k_df))

  expect_equal(test_res$F_stat, as.numeric(lm_method$fstatistic["value"]))
  expect_equal(test_res$df1, as.numeric(lm_method$fstatistic["numdf"]))
  expect_equal(test_res$df2, as.numeric(lm_method$fstatistic["dendf"]))
  expect_equal(test_res$p_value, pf(as.numeric(lm_method$fstatistic["value"]),
                                    lm_method$fstatistic["numdf"],
                                    lm_method$fstatistic["dendf"],
                                    lower.tail = F))
})


test_that("pairwise test", {
  prep_remedi = prepare_data_frame(remedi)
  test_res = hyperbolic_model_f_test(prep_remedi, list(c("EFT", "NCC")))
  est_ln_k_df = get_subj_est_ln_k(prep_remedi)
  lm_method = summary(lm(ln_k~group, data = est_ln_k_df))

  expect_equal(test_res$p_value, lm_method$coefficients["groupNCC", 4])
  expect_equal(test_res$F_stat, lm_method$coefficients["groupNCC", 3]^2)
  expect_equal(test_res$df1, 1)
  expect_equal(test_res$df2, as.numeric(lm_method$fstatistic["dendf"]))
})


test_that("test works when g hat =0", {
  prep_remedi = prepare_data_frame(remedi)

  mean_log_delay = mean(prep_remedi$log_delay[1:7])
  eft_mean = mean(prep_remedi$indiff_transform[prep_remedi$group == "EFT"]) - mean_log_delay
  hit_mean =  mean(prep_remedi$indiff_transform[prep_remedi$group == "HIT"]) - mean_log_delay
  ncc_mean =  mean(prep_remedi$indiff_transform[prep_remedi$group == "NCC"]) - mean_log_delay

  est_hyperparam = c(eft_mean, hit_mean, ncc_mean)
  prep_remedi = merge(prep_remedi, data.frame(group = c("EFT", "HIT", "NCC"), est_hyperparam))
  prep_remedi$fake_ln_k = 0.9*prep_remedi$est_hyperparam + 0.1*prep_remedi$lin_ln_k
  prep_remedi$fake_indiff_transform = prep_remedi$fake_ln_k + prep_remedi$log_delay + prep_remedi$residual_hyperbolic
  prep_remedi$fake_indiff = 1/(1+exp(prep_remedi$fake_indiff_transform))
  fake_remedi = prep_remedi %>%
    select(group, subj, delay, fake_indiff) %>%
    dplyr::rename(indiff = fake_indiff)

  prep_fake_remedi = prepare_data_frame(fake_remedi)

  test_res = hyperbolic_model_f_test(prep_fake_remedi, list(c("EFT", "NCC", "HIT")))
  est_ln_k_df = get_subj_est_ln_k(prep_fake_remedi)
  lm_method = summary(lm(ln_k~group, data = est_ln_k_df))

  expect_equal(test_res$F_stat, as.numeric(lm_method$fstatistic["value"]))
  expect_equal(test_res$df1, as.numeric(lm_method$fstatistic["numdf"]))
  expect_equal(test_res$df2, as.numeric(lm_method$fstatistic["dendf"]))
  expect_equal(test_res$p_value, pf(as.numeric(lm_method$fstatistic["value"]),
                                    lm_method$fstatistic["numdf"],
                                    lm_method$fstatistic["dendf"],
                                    lower.tail = F))

})

test_that("test means agree", {
  prep_remedi = prepare_data_frame(remedi)

  mean_log_delay = mean(prep_remedi$log_delay[1:7])
  eft_mean = mean(prep_remedi$indiff_transform[prep_remedi$group == "EFT"]) - mean_log_delay
  hit_mean =  mean(prep_remedi$indiff_transform[prep_remedi$group == "HIT"]) - mean_log_delay
  ncc_mean =  mean(prep_remedi$indiff_transform[prep_remedi$group == "NCC"]) - mean_log_delay

  mean_params = c(eft_mean, hit_mean, ncc_mean)
  hyp_model_params = estimate_hyperbolic_model_params(prep_remedi)
  expect_equal(hyp_model_params$ln_k_mean$ln_k_mean, mean_params)
})

# TODO: Test the variance components

test_that("sigma squared correct", {
  prep_remedi = prepare_data_frame(remedi)
  hyp_model_params = estimate_hyperbolic_model_params(prep_remedi)

  nest_anova = anova(lm(hyp_left~group+as.factor(subj), data = prep_remedi))
  est_sigma_sq = nest_anova$`Sum Sq`[3]/(434*6)
  est_g = nest_anova$`Sum Sq`[2]/(434*est_sigma_sq) - 1

  expect_equal(as.numeric(hyp_model_params$var[1]), est_sigma_sq)
  expect_equal(as.numeric(hyp_model_params$var[2]), est_g)
})


# TODO: Test the standard errors



# TODO: Test what happens when g should be estimated to be 0.
test_that("var correct g=0", {
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

  hyp_model_params = estimate_hyperbolic_model_params(prep_fake_remedi)

  nest_anova = anova(lm(hyp_left~group+as.factor(subj), data = prep_fake_remedi))

  expect_equal(nest_anova$`Sum Sq`[2]/nest_anova$`Sum Sq`[3]*6-1 <= 0, TRUE)
  est_sigma_sq = (nest_anova$`Sum Sq`[2] + nest_anova$`Sum Sq`[3])/(434*7)
  expect_equal(as.numeric(hyp_model_params$var[1]), est_sigma_sq)
})

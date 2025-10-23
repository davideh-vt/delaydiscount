# First, make a function that can estimate model parameters
# TODO: Come up with a way of estimating standard error for the group ln(k) mean hyperparameters
estimate.hyperbolic.model.params <- function(dd_data){
  # Setup input to get it into predictable format
  #dd_data = prepare_data_frame(dd_data)
  # Get the groups
  dd_data$group = as.factor(dd_data$group)
  groups = levels(dd_data$group)
  # Get the full group model
  group_full_model <- lm(indiff_transform ~ offset(log_delay) + group, data = dd_data)
  ln_k_means = group_full_model$coefficients[1] + c(0, group_full_model$coefficients[-1])
  names(ln_k_means) = groups

  # Estimate the variance parameters
  #  First, get relevant sum square errors
  sse_x = sum(residuals(group_full_model)^2)

  dd_data$group_subj_comb = paste(dd_data$group, dd_data$subj)
  # TODO: Check that there are no spaces in any of the group or subject strings

  subj_model = lm(indiff_transform ~ offset(log_delay) + group_subj_comb, data = dd_data)
  sse_z = sum(residuals(subj_model)^2)
  ssr_z_x = sse_x - sse_z

  # Count various parameters
  n = length(levels(as.factor(dd_data$group_subj_comb)))
  n_tp = length(levels(as.factor(dd_data$delay)))

  g_hat = ssr_z_x/sse_z*(n_tp - 1) - 1
  g_hat = ifelse(g_hat < 0, 0, g_hat)

  sigma_sq = ifelse(g_hat > 0,
                    sse_z/(n*(n_tp-1)),
                    (sse_z + ssr_z_x)/(n*n_tp))

  vars = c(sigma_sq, g_hat)
  names(vars) = c("sigma_sq", "g_hat")

  # Get standard errors
  group_subj_comb = unique(data.frame(group = dd_data$group, subj = dd_data$subj))
  grp_ct = group_subj_comb %>%
    group_by(group) %>%
    summarise(count = n())
  std_errs = sqrt((g+1)*sigma_sq/(grp_ct$count*n_tp))
  names(std_errs) = grp_ct$group

  # TODO: Consolidate ln_k_mean and std_errs into one dataframe
  condition_stats = data.frame(condition = groups, ln_k_mean = ln_k_means[groups], std_err = std_errs[groups])

  return(list(ln_k_mean = condition_stats, var = vars))
  # This used to return the objects ln_k_means and std_errs
}

# Example
# estimate.hyperbolic.model.params(remedi_data_rc)


# Maybe hide this function and make a simpler function that publicly faces the user
#  That function would cover more practical cases but call this function.
# The R Package function that the user is encouraged to use should probably not be this flexible.
hyperbolic.model.f.test <- function(dd_data, hypothesis){
  # Add a check to make sure that a group does not appear in more than one vector
  red_groups = dd_data$group
  for(eq in hypothesis){
    red_groups = ifelse(red_groups %in% eq, eq[1], red_groups)
  }
  dd_data$red_group = red_groups
  # Now get a model for full groups
  group_full_model <- lm(indiff_transform ~ offset(log_delay) + group, data = dd_data)
  sse_x_full = sum(residuals(group_full_model)^2)

  # Do the ifelse
  if(length(levels(as.factor(dd_data$red_group))) == 1){
    group_red_model <- lm(indiff_transform ~ offset(log_delay), data = dd_data)
  } else{
    group_red_model <- lm(indiff_transform ~ offset(log_delay) + red_group, data = dd_data)
  }
  sse_x_red = sum(residuals(group_red_model)^2)

  dd_data$group_subj_comb = paste(dd_data$group, dd_data$subj)
  # TODO: Check that there are no spaces in any of the group or subject strings
  subj_model = lm(indiff_transform ~ offset(log_delay) + group_subj_comb, data = dd_data)
  sse_z = sum(residuals(subj_model)^2)

  ssr_full_red = sse_x_red - sse_x_full
  ssr_z_x_full = sse_x_full - sse_z

  df_1 = group_red_model$df.residual - group_full_model$df.residual
  df_2 = group_full_model$df.residual - subj_model$df.residual

  msr_full_red = ssr_full_red/df_1
  msr_z_x_full = ssr_z_x_full/df_2

  f_stat = msr_full_red/msr_z_x_full
  p_val = pf(f_stat, df_1, df_2, lower.tail = F)

  # Maybe convert to list
  #sig_test = c(f_stat, p_val, df_1, df_2)
  #names(sig_test) = c("F_stat", "p_value", "df1", "df2")

  sig_test = list(F_stat = f_stat,
                  p_value = p_val,
                  df1 = df_1,
                  df2 = df_2)

  return(sig_test)
}

# Maybe have a summary function that takes the output from this function and makes it into a nice table.
# Look at classes in R

#hyperbolic.model.f.test(remedi_data_rc, list(c("EFT", "NCC")))
#hyperbolic.model.f.test(remedi_data_rc, list(c("EFT", "HIT")))
#hyperbolic.model.f.test(remedi_data_rc, list(c("NCC", "HIT")))
#hyperbolic.model.f.test(remedi_data_rc, list(c("NCC", "HIT", "EFT")))


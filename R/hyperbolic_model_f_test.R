#' Perform an F-test for equality of some combination(s) of hyperparameters.
#'
#' This function is a helper function for dd_hyperbolic_model.
#' We recommend not using this function on its own.
#'
#' @importFrom stats lm
#' @importFrom stats residuals
#' @importFrom stats pf
#'
#' @param dd_data A specially formatted data frame as returned by the
#' prepare_data_frame function.
#' @param hypothesis A list of string vectors. Each string is the name of a
#' group. No group appears in more than one vector. Each vector represents a set
#' of hyperparameters that are assumed to be equal under the null hypothesis.
#' @returns A list with the entries
#' F_stat, the test statistic of the F-test;
#' p_value, the p-value of the F-test;
#' df1, the numerator degrees of freedom of the F-test (dimensionality of H0);
#' df2, the denominator degrees of freedom of the F-test.
#'
#' @examples
#' prep_remedi <- prepare_data_frame(remedi)
#' f_test_result <- hyperbolic_model_f_test(prep_remedi, list(c("EFT", "NCC")))
#'
#' @export


hyperbolic_model_f_test <- function(dd_data, hypothesis){
  # assign all subjects within groups assumed to be equal to each other to the
  #  same group level in the reduced model
  red_groups <- dd_data$group
  for(eq in hypothesis){
    red_groups <- ifelse(red_groups %in% eq, eq[1], red_groups)
  }
  dd_data$red_group <- red_groups

  # fit the full model
  group_full_model <- lm(indiff_transform ~ offset(log_delay) + group, data = dd_data)
  sse_x_full = sum(residuals(group_full_model)^2)

  # fit the reduced model
  if(length(levels(as.factor(dd_data$red_group))) == 1){
    group_red_model <- lm(indiff_transform ~ offset(log_delay), data = dd_data)
  } else{
    group_red_model <- lm(indiff_transform ~ offset(log_delay) + red_group, data = dd_data)
  }
  sse_x_red = sum(residuals(group_red_model)^2)

  # combine group and subject into a single category
  dd_data$group_subj_comb = paste(dd_data$group, dd_data$subj)

  # fit subject(group) model
  subj_model = lm(indiff_transform ~ offset(log_delay) + group_subj_comb, data = dd_data)
  sse_z = sum(residuals(subj_model)^2)

  # compute important quadratic forms
  ssr_full_red = sse_x_red - sse_x_full
  ssr_z_x_full = sse_x_full - sse_z

  # degrees of freedom
  df_1 = group_red_model$df.residual - group_full_model$df.residual
  df_2 = group_full_model$df.residual - subj_model$df.residual

  # mean squares
  msr_full_red = ssr_full_red/df_1
  msr_z_x_full = ssr_z_x_full/df_2

  f_stat = msr_full_red/msr_z_x_full
  p_val = pf(f_stat, df_1, df_2, lower.tail = F)

  sig_test = list(F_stat = f_stat,
                  p_value = p_val,
                  df1 = df_1,
                  df2 = df_2)

  return(sig_test)
}

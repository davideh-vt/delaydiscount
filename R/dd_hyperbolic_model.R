#' This function gets model parameter estimates for each group, including hyperparameters
#' and estimates of the variance components (the observation error and subject random effect).
#' It also gets F-test results for pairwise equality for the hyperparameter for
#' each pair of groups, and overall equality of all hyperparameters.
#'
#' @param dd_data A specially formatted data frame as returned by the
#' prepare_data_frame function.
#'
#' @returns A list with the objects
#' ln_k_mean is a data frame with estimates of the hyperparameters of each group
#' along with the standard error of the estimate. The hyperparameter for each
#' group can be interpreted as the mean ln(k) for a subject in that group.
#' var is a vector with entries sigma_sq and g
#'  sigma_sq is the variance of the observed transformed indifference
#'  conditioned on the true ln(k) for the subject
#'  g is related to the variance of the subject random effect, which is equal to
#'  g*sigma_sq/T, where T is the number of time points.
#'
#' @examples
#' prep_remedi <- prepare_data_frame(remedi)
#' dd_model <- dd_hyperbolic_model(prep_remedi)
#'
#' @export


dd_hyperbolic_model <- function(dd_data){
  # Check that the function is being called on prepared output
  if(length(which(unique(names(dd_data)) %in% c("subj", "group", "delay", "indiff", "log_delay", "indiff_transform",
                                                "hyp_left", "lin_ln_k", "residual_hyperbolic"))) < 9){
    stop("The input data frame should be output from the prepare_data_frame function. Extra variables may be added to the output, but preexisting variables must not be deleted, modified, or renamed.")
  }

  fixed_effects_var_ests = estimate_hyperbolic_model_params(dd_data)

  # first, get all groups
  groups = levels(as.factor(dd_data$group))
  n = length(unique(dd_data[,c("subj", "group")])$subj)
  if(length(groups) == 1 | length(groups) == n){
    # if there is only one group, then skip the F-testing
    # same if there is only one subject in each group
    return(fixed_effects_var_ests)
  }
  # f-test results should be in a dataframe
  n_groups <- length(groups)
  num_tests <- n_groups*(n_groups-1)/2
  # cond_1 <- rep(groups, each = (n_groups-1):0)
  cond_1 <- rep("", each = num_tests)
  cond_2 <- rep("", each = num_tests)
  F_stat <- rep(-1, each = num_tests)
  p_value <- rep(-1, each = num_tests)
  df1 <- rep(-1, each = num_tests)
  df2 <- rep(-1, each = num_tests)
  # Do a pairwise f-test for all groups
  test_ct <- 1
  for(i in 1:(length(groups)-1)){
    for(j in (i+1):length(groups)){
      cond_1[test_ct] <- groups[i]
      cond_2[test_ct] <- groups[j]

      hyp = list(groups[c(i,j)])
      f_test = hyperbolic_model_f_test(dd_data, hyp)

      F_stat[test_ct] = f_test$F_stat
      p_value[test_ct] = f_test$p_value
      df1[test_ct] = f_test$df1
      df2[test_ct] = f_test$df2

      test_ct <- test_ct + 1
    }
  }

  pairwise_f_tests <- data.frame(cond_1, cond_2, F_stat, p_value, df1, df2)

  #basic anova (all equal to each other)
  f_test = hyperbolic_model_f_test(dd_data, list(groups))

  result = fixed_effects_var_ests

  result$pairwise_f_tests = pairwise_f_tests
  result$model_test = data.frame(F_stat = f_test$F_stat, p_value = f_test$p_value, df1 = f_test$df1, df2 = f_test$df2)

  return(result)
}

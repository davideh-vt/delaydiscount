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
#' @export


dd_hyperbolic_model <- function(dd_data){
  # TODO: Document this function
  # TODO: The object returned by this function should have its own class maybe?
  # TODO: Summary function or maybe override print method when I assign a class

  # Check that the function is being called on prepared output
  if(length(which(unique(names(dd_data)) %in% c("subj", "group", "delay", "indiff", "log_delay", "indiff_transform",
                                                "hyp_left", "lin_ln_k", "residual_hyperbolic"))) < 9){
    stop("The input data frame should be output from the prepare_data_frame function. Extra variables may be added to the output, but preexisting variables must not be deleted, modified, or renamed.")
  }

  fixed_effects_var_ests = estimate_hyperbolic_model_params(dd_data)

  # first, get all groups
  groups = levels(as.factor(dd_data$group))
  # f-test results should be in a dataframe
  cond_1 <- c()
  cond_2 <- c()
  F_stat <- c()
  p_value <- c()
  df1 <- c()
  df2 <- c()
  # Do a pairwise f-test for all groups
  for(i in 1:(length(groups)-1)){
    for(j in (i+1):length(groups)){
      cond_1 <- c(cond_1, groups[i])
      cond_2 <- c(cond_2, groups[j])

      hyp = list(groups[c(i,j)])
      f_test = hyperbolic_model_f_test(dd_data, hyp)

      F_stat = c(F_stat, f_test$F_stat)
      p_value = c(p_value, f_test$p_value)
      df1 = c(df1, f_test$df1)
      df2 = c(df2, f_test$df2)
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

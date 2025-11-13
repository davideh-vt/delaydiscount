dd_hyperbolic_model <- function(dd_data){
  # TODO: The object returned by this function should have its own class maybe?
  # TODO: Summary function or maybe override print method when I assign a class

  fixed_effects_var_ests = estimate.hyperbolic.model.params(dd_data)

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
      f_test = hyperbolic.model.f.test(dd_data, hyp)

      F_stat = c(F_stat, f_test$F_stat)
      p_value = c(p_value, f_test$p_value)
      df1 = c(df1, f_test$df1)
      df2 = c(df2, f_test$df2)
    }
  }

  pairwise_f_tests <- data.frame(cond_1, cond_2, F_stat, p_value, df1, df2)

  #basic anova (all equal to each other)
  f_test = hyperbolic.model.f.test(dd_data, list(groups))

  result = fixed_effects_var_ests

  result$pairwise_f_tests = pairwise_f_tests
  result$model_test = data.frame(F_stat = f_test$F_stat, p_value = f_test$p_value, df1 = f_test$df1, df2 = f_test$df2)

  return(result)
}

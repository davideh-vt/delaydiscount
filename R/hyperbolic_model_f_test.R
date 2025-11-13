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


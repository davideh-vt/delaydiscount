# TODO: Clean up a lot of the crap such as the commented out code
# TODO: Maybe don't modify the input

prepare_data_frame <- function(dd_data){
  # first, make sure that it does not have extra variables
  dd_data <- dd_data %>%
    dplyr::select(study, subj, group, delay, indiff)

  dd_data = dd_data %>%
    arrange(study, group, subj, delay)

  # add transformations of variables
  dd_data$log_delay <- log(dd_data$delay)
  dd_data$indiff_transform <- log(1/dd_data$indiff - 1)

  # prepare variables for hyperbolic model
  dd_data$hyp_left <- dd_data$indiff_transform - dd_data$log_delay

  # calculate est ln_k by subj
  # make sure this works
  est_ln_k <- dd_data %>%
    group_by(study, group, subj) %>%
    summarise(lin_ln_k = mean(hyp_left))
  dd_data = merge(dd_data, est_ln_k) %>%
    arrange(study, group, subj, delay)
  # Calculate residuals
  #combination <- paste(dd_data$study, dd_data$subj, dd_data$group)
  #hyperbolic_model <- lm(dd_data$indiff_transform ~ offset(dd_data$log_delay) + combination)
  #dd_data$residual_hyperbolic <- residuals(hyperbolic_model)
  # TODO: make the function that calculates the ln_k under the hyperbolic model for all subjects within group
  dd_data$residual_hyperbolic <- dd_data$hyp_left - dd_data$lin_ln_k

  # prepare variables for hyperboloid model
  # todo: find another way to do this
  #all_subjs_model <- lm(dd_data$indiff_transform ~ dd_data$log_delay*combination)
  #dd_data$residual <- residuals(all_subjs_model)

  return(dd_data)
}

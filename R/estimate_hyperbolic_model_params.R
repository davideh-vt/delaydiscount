#' Estimate the hyperparameters for each group, as well as the variance
#' components of the model, using maximum likelihood estimation.
#'
#' #' This function is a helper function for dd_hyperbolic_model.
#' We recommend not using this function on its own.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%

#'
#' @param dd_data A specially formatted data frame as returned by the
#' prepare_data_frame function.
#' @returns A list of two objects
#' ln_k_mean is a data frame with estimates of the hyperparameters of each group
#' along with the standard error of the estimate. The hyperparameter for each
#' group can be interpreted as the mean ln(k) for a subject in that group.
#' vars is a vector with entries sigma_sq and g
#'  sigma_sq is the variance of the observed transformed indifference
#'  conditioned on the true ln(k) for the subject
#'  g is related to the variance of the subject random effect, which is equal to
#'  g*sigma_sq/T, where T is the number of time points.

# First, make a function that can estimate model parameters
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
  names(vars) = c("sigma_sq", "g")

  # Get standard errors
  group_subj_comb = unique(data.frame(group = dd_data$group, subj = dd_data$subj))
  grp_ct = group_subj_comb %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(count = n())
  std_errs = sqrt((g_hat+1)*sigma_sq/(grp_ct$count*n_tp))
  names(std_errs) = grp_ct$group

  # TODO: Consolidate ln_k_mean and std_errs into one dataframe
  condition_stats = data.frame(condition = groups, ln_k_mean = ln_k_means[groups], std_err = std_errs[groups])

  return(list(ln_k_mean = condition_stats, var = vars))
  # This used to return the objects ln_k_means and std_errs
}

# Example
# estimate.hyperbolic.model.params(remedi_data_rc)

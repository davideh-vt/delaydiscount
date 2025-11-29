#' Check preconditions for an input data frame for the prepare_data_frame and
#' jb_rule_check functions.
#'
#' This function is a helper function for the prepare_data_frame and
#' jb_rule_check methods.
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom magrittr %>%
#'
#' @param dd_data A data frame containing discounting data in long format.
#' This function is designed to fail if the preconditions for the discounting
#' data data frame are not met.


check_input_preconditions <- function(dd_data){
  # Check that the data frame has subj, group, delay, and indiff columns
  if(length(which(unique(names(dd_data)) %in% c("subj", "group", "delay", "indiff"))) < 4){
    stop("The input data frame must have columns for subj, group, delay, and indiff")
  }

  # Check that all subject(group)s have observations for the same set of delays,
  #  and that there are no duplicate observations.
  delay_set = unique(dd_data$delay)
  n_tp = length(delay_set)
  id_df = dd_data %>%
    select(subj, group, delay) %>%
    unique()
  if(dim(id_df)[1] != dim(dd_data)[1]){
    # This checks to ensure there are no repeat observations
    stop("The input data frame must have no more than one observation per time point for each subject within group.")
  }
  n_obs_id = id_df %>%
    group_by(subj, group) %>%
    summarise(n_tp = n())
  if(sum(n_obs_id$n_tp != n_tp) > 0){
    stop("Every subject within group must have an observation for each time point.")
  }

  # Check that all delays are positive
  if(sum(delay_set <= 0) > 0){
    stop("All delays must be positive.")
  }

  # Check that all indifference points are between 0 and 1, exclusive
  if(sum(dd_data$indiff <= 0) + sum(dd_data$indiff >= 1) > 0){
    stop("All indifference points must be between 0 and 1, exclusive.")
  }
}

#' Apply rule checks to each subject.
#'
#' The rules are:
#' 1. There must be no more than one pair of consecutive time points in which
#' the indifference point increases by more than 0.2
#' 2. The indifference point must decrease by at least 0.1 from the earliest
#' to the latest time point.
#'
#' @param dd_data A data frame containing discounting data in long format.
#' (describe preconditions for the input?)
#' The data should be in long format, with columns identifying the subject,
#' group and time point for each observation, and the indifference point.
#' The variables should be named subj, group, delay, and indiff, respectively.
#' Values for indifference points should be between 0 and 1, exclusive.
#' All delays should be positive.
#' All subjects should have observations for the exact same set of delays.
#'
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
#'
#' @returns A data frame consisting of one observation per subject within group
#' with the boolean variables C1 and C2 which are TRUE if the corresponding rule
#' was passed and FALSE if it was not.
#'
jb_rule_check <- function(dd_data){
  # Check to make sure that the dd_data_1 produced by this procedure is
  #  the same as that produced by the reshape function

  # Check that input passes preconditions
  check_input_preconditions(dd_data)

  n_tp = length(unique(dd_data$delay))
  dd_data_1 <- dd_data %>%
    select(group, subj, delay, indiff) %>%
    arrange(group, subj, delay)

  id_vars = which(names(dd_data_1) %in% c("group", "subj"))
  id_order = unique(dd_data_1[,id_vars])
  indiff_mat <- t(matrix(data = dd_data_1$indiff,
                         nrow = n_tp,
                         ncol = length(id_order[,1])))

  tp_diff = indiff_mat[,2:n_tp] - indiff_mat[,1:(n_tp-1)]
  jumps = tp_diff >= .2
  jump_count = rowSums(jumps)
  C1 = jump_count < 2
  C2 = indiff_mat[,1] - indiff_mat[,n_tp] >= .1

  dd_data_1 <- id_order
  dd_data_1$C1 = C1
  dd_data_1$C2 = C2

  return(dd_data_1)
}

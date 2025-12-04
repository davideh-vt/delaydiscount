#' Return ln(k) estimates for each subject
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#'
#' @param dd_data A specially formatted data frame as returned by the
#' prepare_data_frame function.
#' @returns  A data frame consisting of one observation per subject within group
#' with the variable ln_k containing that subject's estimated ln_k value.
get_subj_est_ln_k <- function(dd_data){
  result <- dd_data %>%
    group_by(group, subj) %>%
    summarise(ln_k = mean(hyp_left))

  return(result)
}

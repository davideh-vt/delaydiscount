#' Return ln(k) estimates for each subject
#'
#' @param dd_data A specially formatted data frame as returned by the
#' prepare_data_frame function.
#' @returns  A data frame consisting of one observation per subject within group
#' with the variable ln_k containing that subject's estimated ln_k value.
get_subj_est_ln_k <- function(dd_data){
  if("study" %in% names(dd_data)){
    result <- dd_data %>%
      group_by(study, group, subj)
  } else{
    result <- dd_data %>%
      group_by(group, subj)
  }

  result <- result %>%
    summarise(ln_k = mean(hyp_left))

  return(result)
}

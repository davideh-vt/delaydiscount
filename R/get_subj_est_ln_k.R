# Return ln(k) estimates for each subject
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

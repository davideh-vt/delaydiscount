# This function depends on dplyr. Is this reasonable?

jb_rule_check <- function(dd_data){
  # Check to make sure that the dd_data_1 produced by this procedure is
  #  the same as that produced by the reshape function

  # Check if the study variable is included in the dataset.
  study_included = "study" %in% names(dd_data)

  if(!study_included){
    dd_data$study = 1
  }
  id_vars = which(names(dd_data) %in% c("study", "group", "subj"))

  n_tp = length(unique(dd_data$delay))
  dd_data_1 <- dd_data %>%
    select(study, group, subj, delay, indiff) %>%
    arrange(study, group, subj)
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

  if(!study_included){
    dd_data_1 <- dd_data_1 %>%
      select(group, subj, C1, C2)
  }
  return(dd_data_1)
}

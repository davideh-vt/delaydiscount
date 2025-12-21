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
#'
#' @export
get_subj_est_ln_k <- function(dd_data){
  # Check that the data frame is compatible
  if(length(which(unique(names(dd_data)) %in% c("subj", "group", "delay", "indiff", "log_delay", "indiff_transform",
                                                "hyp_left", "lin_ln_k", "residual_hyperbolic"))) < 9){
    stop("The input data frame should be output from the prepare_data_frame function. Extra variables may be added to the output, but preexisting variables must not be deleted, modified, or renamed.")
  }

  result <- dd_data %>%
    group_by(group, subj) %>%
    summarise(ln_k = mean(hyp_left))

  return(result)
}

#' Prepare a data frame containing discounting for use on the other functions
#' in this package.
#'
#' This includes:
#' Reducing the data frame to only contain the expected variables.
#' Sorting the observations into an expected order.
#' Transforming delay and indifference for the linearized model.
#' Estimating ln(k) for each subject.
#' Calculating residuals from observed values vs predictions by estimated ln(k).
#'
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @param dd_data A data frame containing discounting data in long format.
#' The data should be in long format, with columns identifying the subject,
#' group and time point for each observation, and the indifference point.
#' The variables should be named subj, group, delay, and indiff, respectively.
#' Values for indifference points should be between 0 and 1, exclusive.
#' All delays should be positive.
#' All subjects should have observations for the exact same set of delays.
#'
#' @returns A data frame compatible with the other functions in this package.
#'
#' @examples
#' prep_remedi <- prepare_data_frame(remedi)
#'
#' @export


prepare_data_frame <- function(dd_data){
  # Check that input passes preconditions
  check_input_preconditions(dd_data)

  # first, make sure that it does not have extra variables
  dd_data <- dd_data %>%
    dplyr::select(.data$subj, .data$group, .data$delay, .data$indiff) %>%
    dplyr::arrange(.data$group, .data$subj, .data$delay)

  # add transformations of variables
  dd_data$log_delay <- log(dd_data$delay)
  dd_data$indiff_transform <- log(1/dd_data$indiff - 1)

  # prepare variables for hyperbolic model
  dd_data$hyp_left <- dd_data$indiff_transform - dd_data$log_delay

  # calculate est ln_k by subj
  est_ln_k <- dd_data %>%
    dplyr::group_by(.data$group, .data$subj) %>%
    dplyr::summarise(lin_ln_k = mean(.data$hyp_left))
  dd_data = merge(dd_data, est_ln_k) %>%
    dplyr::arrange(.data$group, .data$subj, .data$delay)
  # Calculate residuals
  dd_data$residual_hyperbolic <- dd_data$hyp_left - dd_data$lin_ln_k

  return(dd_data)
}

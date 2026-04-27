#' Simulate a dataset from the hierarchical linearized hyperbolic model.
#'
#' @param conditions A character vector, with each component naming a condition.
#' @param num_subj An integer vector the same length as the vector of conditions.
#' Each entry represents the number of subjects in the respective condition.
#' @param time_points A vector of positive numbers in increasing order, representing
#'  the time points at which a subject's delay discounting rate is measured.
#' @param mean_ln_k A numeric vector of the same length as the vector of conditions.
#'  Each value represents the population ln_k mean for that condition.
#' @param sigma_sq The variance of an observed indifference points's transformed
#'  value, conditional on the variance.
#' @param g Parameter controlling the variance of individual subject ln k parameters.
#' Equal to Var(ln_k)*(Number of time points)/sigma_sq, that is, the ratio of the
#' variance of a subject ln k parameter to the variance of the estimate of a
#' subject ln k parameter (conditional on the true ln k parameter).
#'
#' @returns A data frame of simulated delay discounting data containing one observation
#'  per delay per subject. It contains the following columns:
#'  subj: A number identifying the subject
#'  true_ln_k: The true ln_k parameter of that subject
#'  group: The subject's condition
#'  delay: The delay for the observation.
#'  indiff: The indifference point for the subject at the delay the observation
#'  corresponds to, between 0 and 1, representing the proportion of the reward
#'  the subject would need to receive to choose receiving the smaller reward now
#'  instead of waiting the delay for the full reward.
#' @examples
#' dd_data <- simulate_dataset(conditions = c("EFT", "NCC"),
#'  num_subj = c(75, 150),
#'  time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
#'  mean_ln_k = c(-6.7, -6),
#'  sigma_sq = 1.5, g = 10)
#'
#' @export

simulate_dataset <- function(conditions,
                             num_subj,
                             time_points,
                             mean_ln_k,
                             sigma_sq, g){

  num_subj <- floor(num_subj)
  if(length(conditions) != length(unique(conditions))){
    stop("All conditions must be unique.")
  }
  if(length(conditions) != length(num_subj)){
    stop("num_subj must have length equal to the number of conditions.")
  }
  if(length(conditions) != length(mean_ln_k)){
    stop("mean_ln_k must have length equal to the number of conditions.")
  }
  if(sum(num_subj > 0) != length(num_subj)){
    stop("Numbers of subjects must be positive.")
  }
  if(sigma_sq < 0 | g < 0){
    stop("Variance components sigma_sq and g must not be negative.")
  }

  # Get number of time points
  n_tp <- length(time_points)
  # Calculate variance of the true ln_k
  var_ln_k <- g*sigma_sq/n_tp

  # Set up output
  result <- data.frame(matrix(nrow = n_tp*sum(num_subj), ncol = 5))
  names(result) <- c("subj", "true_ln_k", "group", "delay", "indiff")

  cond_index <- c(0, cumsum(num_subj))

  for(i in 1:length(conditions)){
    n_cond <- num_subj[i]  # Number of subjects for this condition

    # Simulate the true ln_k
    true_ln_k <- rnorm(n_cond, mean = mean_ln_k[i], sd = sqrt(var_ln_k))

    # Get all the terms that compose indiff_transform
    ln_k_term <- rep(true_ln_k, each = n_tp)
    time_term <- rep(log(time_points), n_cond)
    random_term <- rnorm(n_cond*n_tp, mean = 0, sd = sqrt(sigma_sq))

    indiff_transform <- ln_k_term + time_term + random_term

    # Get original indiff
    indiff <- 1/(exp(indiff_transform) + 1)

    group_df <- data.frame(subj = rep((cond_index[i]+1):cond_index[i+1], each = n_tp),
                           true_ln_k = rep(true_ln_k, each = n_tp),
                           group = conditions[i],
                           delay = rep(time_points, n_cond),
                           indiff = indiff)

    result[(n_tp*cond_index[i]+1):(n_tp*cond_index[i+1]),] <- group_df
  }
  return(result)
}

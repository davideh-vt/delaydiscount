simulate_dataset <- function(conditions = c("EFT", "NCC"),
                             num_subj = c(75, 150),
                             time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
                             mean_ln_k = c(-6.7, -6),
                             sigma_sq = 1.5, g = 10){

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

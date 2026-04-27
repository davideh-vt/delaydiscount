# Simulate a dataset from the hierarchical linearized hyperbolic model.

Simulate a dataset from the hierarchical linearized hyperbolic model.

## Usage

``` r
simulate_dataset(conditions, num_subj, time_points, mean_ln_k, sigma_sq, g)
```

## Arguments

- conditions:

  A character vector, with each component naming a condition.

- num_subj:

  An integer vector the same length as the vector of conditions. Each
  entry represents the number of subjects in the respective condition.

- time_points:

  A vector of positive numbers in increasing order, representing the
  time points at which a subject's delay discounting rate is measured.

- mean_ln_k:

  A numeric vector of the same length as the vector of conditions. Each
  value represents the population ln_k mean for that condition.

- sigma_sq:

  The variance of an observed indifference points's transformed value,
  conditional on the variance.

- g:

  Parameter controlling the variance of individual subject ln k
  parameters. Equal to Var(ln_k)\*(Number of time points)/sigma_sq, that
  is, the ratio of the variance of a subject ln k parameter to the
  variance of the estimate of a subject ln k parameter (conditional on
  the true ln k parameter).

## Value

A data frame of simulated delay discounting data containing one
observation per delay per subject. It contains the following columns:
subj: A number identifying the subject true_ln_k: The true ln_k
parameter of that subject group: The subject's condition delay: The
delay for the observation. indiff: The indifference point for the
subject at the delay the observation corresponds to, between 0 and 1,
representing the proportion of the reward the subject would need to
receive to choose receiving the smaller reward now instead of waiting
the delay for the full reward.

## Examples

``` r
dd_data <- simulate_dataset(conditions = c("EFT", "NCC"),
 num_subj = c(75, 150),
 time_points = c(30, 90, 180, 365, 1095, 1825, 3650),
 mean_ln_k = c(-6.7, -6),
 sigma_sq = 1.5, g = 10)
```

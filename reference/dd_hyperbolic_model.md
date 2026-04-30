# This function gets model parameter estimates for each group, including hyperparameters and estimates of the variance components (the observation error and subject random effect). It also gets F-test results for pairwise equality for the hyperparameter for each pair of groups, and overall equality of all hyperparameters.

This function gets model parameter estimates for each group, including
hyperparameters and estimates of the variance components (the
observation error and subject random effect). It also gets F-test
results for pairwise equality for the hyperparameter for each pair of
groups, and overall equality of all hyperparameters.

## Usage

``` r
dd_hyperbolic_model(dd_data)
```

## Arguments

- dd_data:

  A specially formatted data frame as returned by the prepare_data_frame
  function.

## Value

A list with the objects ln_k_mean is a data frame with estimates of the
hyperparameters of each group along with the standard error of the
estimate. The hyperparameter for each group can be interpreted as the
mean ln(k) for a subject in that group. var is a vector with entries
sigma_sq and g. sigma_sq is the variance of the observed transformed
indifference conditioned on the true ln(k) for the subject g is related
to the variance of the subject random effect, which is equal to
g\*sigma_sq/T, where T is the number of time points. pairwise_f_tests is
a data frame containing the results of all of the pairwise f-tests for
equality of hyperparameters. The columns col1 and col2 contain the
groups of the pairwise F-test. The F_stat column contains the
F-statistic, the p_value column contains the p-value, and the df1 and
df2 columns contain the numerator and denominator degrees of freedom,
respectively. model_test is a data frame containing the result of the
F-test for equality of all hyperparameters. It contains the columns
F_stat, p_value, df1, and df2, which have the same meaning as in the
pairwise_f_tests data frame.

## Examples

``` r
prep_remedi <- prepare_data_frame(remedi)
dd_model <- dd_hyperbolic_model(prep_remedi)
```

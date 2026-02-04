# Perform an F-test for equality of some combination(s) of hyperparameters.

This function is a helper function for dd_hyperbolic_model. We recommend
not using this function on its own.

## Usage

``` r
hyperbolic_model_f_test(dd_data, hypothesis)
```

## Arguments

- dd_data:

  A specially formatted data frame as returned by the prepare_data_frame
  function.

- hypothesis:

  A list of string vectors. Each string is the name of a group. No group
  appears in more than one vector. Each vector represents a set of
  hyperparameters that are assumed to be equal under the null
  hypothesis.

## Value

A list with the entries F_stat, the test statistic of the F-test,
p_value, the p-value of the F-test df1, the numerator degrees of freedom
of the F-test (dimensionality of H0) df2, the denominator degrees of
freedom of the F-test

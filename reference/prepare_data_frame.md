# Prepare a data frame containing discounting for use on the other functions in this package.

This includes: Reducing the data frame to only contain the expected
variables. Sorting the observations into an expected order. Transforming
delay and indifference for the linearized model. Estimating ln(k) for
each subject. Calculating residuals from observed values vs predictions
by estimated ln(k)

## Usage

``` r
prepare_data_frame(dd_data)
```

## Arguments

- dd_data:

  A data frame containing discounting data in long format. (describe
  preconditions for the input?) The data should be in long format, with
  columns identifying the subject, group and time point for each
  observation, and the indifference point. The variables should be named
  subj, group, delay, and indiff, respectively. Values for indifference
  points should be between 0 and 1, exclusive. All delays should be
  positive. All subjects should have observations for the exact same set
  of delays.

## Value

A data frame compatible with the other functions in this package.

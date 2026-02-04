# Apply rule checks to each subject.

The rules are:

1.  There must be no more than one pair of consecutive time points in
    which the indifference point increases by more than 0.2

2.  The indifference point must decrease by at least 0.1 from the
    earliest to the latest time point.

## Usage

``` r
jb_rule_check(dd_data)
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

A data frame consisting of one observation per subject within group with
the boolean variables C1 and C2 which are TRUE if the corresponding rule
was passed and FALSE if it was not.

# Return ln(k) estimates for each subject

Return ln(k) estimates for each subject

## Usage

``` r
get_subj_est_ln_k(dd_data)
```

## Arguments

- dd_data:

  A specially formatted data frame as returned by the prepare_data_frame
  function.

## Value

A data frame consisting of one observation per subject within group with
the variable ln_k containing that subject's estimated ln_k value.

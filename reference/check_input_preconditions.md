# Check preconditions for an input data frame for the prepare_data_frame and jb_rule_check functions.

This function is a helper function for the prepare_data_frame and
jb_rule_check methods.

## Usage

``` r
check_input_preconditions(dd_data)
```

## Arguments

- dd_data:

  A data frame containing discounting data in long format. This function
  is designed to fail if the preconditions for the discounting data data
  frame are not met.

## Value

NULL if the input data frame passes the format checks. Throws an error
if an input check is failed.

## Examples

``` r
check_input_preconditions(remedi)
```

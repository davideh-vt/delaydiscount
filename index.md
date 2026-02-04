# delaydiscount

The goal of the delaydiscount package is to provide methods for
analyzing delay discounting data using the linearized hyperbolic model.

## Installation

You can install the development version of delaydiscount from Github.

``` r
# install.packages("devtools")
devtools::install_git("https://github.com/davideh-vt/delaydiscount")
```

## Example

To fit a model on a dataset of delay discounting data, first run
`prepare_data_frame` on the dataset, then run `dd_hyperbolic_model` on
the output.

``` r
library(delaydiscount)
## basic example of fitting a model
prep_remedi <- prepare_data_frame(remedi)
fit <- dd_hyperbolic_model(prep_remedi)
fit$ln_k_mean
#>     condition ln_k_mean   std_err
#> EFT       EFT -6.877057 0.1638994
#> HIT       HIT -5.860534 0.1506689
#> NCC       NCC -6.078229 0.1369001
fit$var
#>  sigma_sq         g 
#>  1.978107 10.407330
```

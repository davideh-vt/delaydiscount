# delaydiscount

``` r
library(delaydiscount)
```

The delaydiscount package is intended to be used to analyze delay
discounting data using the linearized hyperbolic model proposed by
Hinds, et al. (2026). To analyze discounting data with this package, you
should first get your data in the format expected by the package.

Our package expects to take data in long format, meaning that for each
subject within group, for each delay, the measurement of the
indifference point constitutes one observation in the data frame. It
expects the data frame to contain variables named

- group: Intervention identifier.

- subj: Subject identifier.

- delay: The length of time for which the indifference point is
  measured. This must be a positive value.

- indiff: A value between 0 and 1 (exclusive) expressing the proportion
  of a maximum reward the subject would need to receive to not favor
  waiting the delay to receive the maximum reward.

For each subject within group, the indifference point is expected to be
measured for the same set of delays, exactly once for each delay.

Samples are assumed to be independent between different groups. If a
subject is in multiple groups, this assumption would be violated. The
code will still run, but the analysis may not be valid. The analysis
performed by this package would treat a subject in multiple groups as
though the observations for that subject in different groups had come
from different subjects.

We include the remedi dataset in our package as an example of a properly
formatted dataset.

``` r
head(remedi, n = 10L)
#>     study     subj group delay    indiff
#> 1  REMEDI 44864071   HIT    30 0.7109375
#> 2  REMEDI 44864071   HIT    90 0.5078124
#> 3  REMEDI 44864071   HIT   180 0.3984375
#> 4  REMEDI 44864071   HIT   365 0.2421875
#> 5  REMEDI 44864071   HIT  1095 0.2109375
#> 6  REMEDI 44864071   HIT  1825 0.1640624
#> 7  REMEDI 44864071   HIT  3650 0.0859375
#> 8  REMEDI 96649869   NCC    30 0.9296875
#> 9  REMEDI 96649869   NCC    90 0.9609375
#> 10 REMEDI 96649869   NCC   180 0.9609375
```

Once your delay discounting data is in the expected format, the next
step is to run the `prepare_data_frame` function on the dataset, which
performs helper calculations for our other functions.

``` r
prep_remedi <- prepare_data_frame(remedi)
```

One of the things that the `prepare_data_frame` function does is
transform the delay and indifference variables to linearize the model.

Note that the hyperbolic model for the discounting curve is
$D(t) = \frac{1}{1 + kt}$.

Then we will have
$\log\left( \frac{1}{D(t)} - 1 \right) = \log(k) + \log(t)$.

We assume that the transformed observed indifference point
$\widetilde{D}\left( t_{ijc} \right)$ will follow the model

$\log\left( \frac{1}{\widetilde{D}\left( t_{ijc} \right)} - 1 \right) = \log\left( k_{ic} \right) + \log\left( t_{ijc} \right) + \epsilon_{ijc},$

where $\epsilon_{ijc} \sim N\left( 0,\sigma^{2} \right)$, $i$ is an
index for subject, $j$ an index for time point, and $c$ is an index for
group, and

$\log\left( k_{ic} \right) \sim N\left( \theta_{c},\frac{g\sigma^{2}}{T} \right)$,
where $\theta_{c}$ is the population mean of the
$\log\left( k_{ic} \right)$ for subjects in group $c$, and $T$ is the
number of time points observed for each subject.

Using the prepared data frame, you can get estimates of the $log(k)$
values for each subject using the `get_subj_est_ln_k` function.

``` r
ln_k_ests <- get_subj_est_ln_k(prep_remedi)
head(ln_k_ests)
#> # A tibble: 6 × 3
#> # Groups:   group [1]
#>   group     subj   ln_k
#>   <chr>    <int>  <dbl>
#> 1 EFT   10458654  -7.73
#> 2 EFT   10698816  -6.39
#> 3 EFT   11624253  -6.69
#> 4 EFT   11646612  -6.58
#> 5 EFT   12088534  -8.41
#> 6 EFT   12691633 -10.4
```

Our model treats the $\log(k)$ values for individual subjects as the
result of a combination of a fixed group effect and a random subject
effect. To get the fit of the linearized hyperbolic model, use the
`dd_hyperbolic_model` function.

``` r
model_fit <- dd_hyperbolic_model(prep_remedi)
```

The object produced by this method is a list containing several
components.

The group mean $\log(k)$ estimates (i.e. the $\theta_{c}$) can be gotten
from the `ln_k_mean` component.

``` r
model_fit$ln_k_mean
#>     condition ln_k_mean   std_err
#> EFT       EFT -6.877057 0.1638994
#> HIT       HIT -5.860534 0.1506689
#> NCC       NCC -6.078229 0.1369001
```

These are estimates of the mean of the distribution from which the
$\log\left( k_{ic} \right)$ values for subjects in group $c$ have been
realized.

The variance component estimates can be obtained from the `var`
component.

``` r
model_fit$var
#>  sigma_sq         g 
#>  1.978107 10.407330
```

`sigma_sq` is the estimate of the variance of the transformed
indifference value conditional on the subject effect.

`g` is related to the variance of the subject random effects:
specifically, the variance of the subject random effect is equal to
$\frac{g\sigma^{2}}{T}$, where $T$ is the number of time points observed
for each subject.

These mean and variance parameters are estimated through maximum
likelihood estimation.

Pairwise F-tests for equality of group mean parameters can be obtained
from the `pairwise_f_tests` component. The data frame contains an F-test
for each pair.

``` r
model_fit$pairwise_f_tests
#>   cond_1 cond_2    F_stat      p_value df1 df2
#> 1    EFT    HIT 20.704017 6.979837e-06   1 431
#> 2    EFT    NCC 13.895842 2.189874e-04   1 431
#> 3    HIT    NCC  1.135632 2.871738e-01   1 431
```

Do note that these p-values are not adjusted for multiplicity. When
conducting multiple hypothesis tests, the probability of at least one
being significant will be larger than the nominal alpha required for
significance.

An overall F-test for the equality of all group mean parameters can be
found in the `model_test` component.

``` r
model_fit$model_test
#>     F_stat      p_value df1 df2
#> 1 11.33482 1.593828e-05   2 431
```

Note that this test being significant does not necessarily indicate
which group means differ from each other.

## Citation

Hinds, et al. (2026). “To linearize or not to linearize: That is the
Mazur delay discounting question”, submitted to *Journal of Mathematical
Psychology*.

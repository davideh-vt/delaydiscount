# rule-check

``` r
library(delaydiscount)
```

TODO: This file should include the entire workflow and not just the rule
check. However, I do not want to add it until I finish it for the main
path (no rule check), since any edits will need to be copied over.

The jb_rule_check function can be used to apply the rule check for the
purpose of filtering out subjects who fail.

``` r
remedi_rc_result <- jb_rule_check(remedi)
head(remedi_rc_result)
#>    group     subj   C1    C2
#> 1    EFT 10458654 TRUE  TRUE
#> 8    EFT 10698816 TRUE  TRUE
#> 15   EFT 11624253 TRUE  TRUE
#> 22   EFT 11646612 TRUE  TRUE
#> 29   EFT 12088534 TRUE  TRUE
#> 36   EFT 12691633 TRUE FALSE
```

The resulting data frame has an observation for each
subject-within-group, with variables corresponding to whether or not
each rule was passed. A value of TRUE indicates that the rule check was
passed.

C1 fails if there are two or more increases in the indifference point of
20% of the total amount between consecutive time points. C2 fails if the
difference in the indifference point between the first and last time
points is not more than 10% of the total amount.

In order to make use of the rule check results, after the data frame is
prepared, the rule check data can be merged with the original dataframe,
and then observations corresponding to subjects who failed the rule
check can be removed from the data.

``` r
remedi_with_rc <- merge(remedi, remedi_rc_result)
remedi_rc_pass <- dplyr::filter(remedi_with_rc, 
                                C1 == TRUE, C2 == TRUE)
```

After that, the remainder of the workflow proceeds as it would
otherwise.

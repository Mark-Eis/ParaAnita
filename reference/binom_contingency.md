# Binomial Contingency Table for Data with a Binary Outcome

`binom_contingency()` creates a binomial contingency table for data with
a binary dependent variable and one or more categorical independent
variables, optionally including totals, proportions and confidence
intervals.

## Usage

``` r
binom_contingency(
  .data,
  .dep_var,
  ...,
  .drop_zero = FALSE,
  .propci = FALSE,
  .level = 0.95
)

as_binom_contingency(object, ...)

# S3 method for class 'data.frame'
as_binom_contingency(
  object,
  ...,
  .pn = NULL,
  .qn = NULL,
  .drop_zero = FALSE,
  .propci = FALSE,
  .level = 0.95
)
```

## Arguments

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of a binary dependent variable, which should be `numeric`
  with values of `0` and `1`.

- ...:

  for `binomial_contingency()`:
  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  quoted name(s) of one or more `factor` or `character vector` columns
  in `.data`, to be included in (or excluded from) the output.

  for `as_binomial_contingency()`: further arguments passed to or from
  other methods.

- .drop_zero:

  `logical`. If `TRUE`, [`levels`](https://rdrr.io/r/base/levels.html)
  of explanatory factors for which values of `.dep_var` are either all
  zero or all one are dropped from the output; default `FALSE`.

- .propci:

  `logical`. If `TRUE`, each row of the output `"binom_contingency"`
  object includes totals, proportions and confidence intervals; default
  `FALSE`.

- .level:

  the confidence level required; default `0.95`.

- object:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)),
  to be coerced to a `"binom_contingency"` object.

- .pn, .qn:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted names of columns in `object` representing numbers of successes
  and failures in Bernoulli trials; default `NULL`.

## Value

An object of class `"binom_contingency"`, `"announce"`, inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html),
with columns `pn` and `qn` representing the number of "successes" and
"failures" respectively, and further columns for independent
(explanatory) variables.

If `.propci = TRUE` additional columns are output representing totals,
proportions and confidence intervals.

## Details

Categorical variables (i.e. factors or character vectors) in `.data`
required as factors in the resulting contingency table may be selected
for inclusion or exclusion using the `...` argument and the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax from package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of “selection helpers”. If no `...` arguments are
supplied, all categorical variables in `.data` (other than `.dep_var`)
will be used.

A [list](https://rdrr.io/r/base/list.html) of [defused R
expressions](https://rlang.r-lib.org/reference/topic-defuse.html), as
for instance created by
[`expl_fcts()`](https://mark-eis.github.io/ParaAnita/reference/expl_fcts.md),
may be used as the `...` arguments and should be injected using the
[splice-operator](https://rlang.r-lib.org/reference/splice-operator.html),
`!!!`, see examples.

Use `.drop_zero = TRUE` to drop
[`levels`](https://rdrr.io/r/base/levels.html) of explanatory factors
for which values of `.dep_var` are either all zero or all one, to
prevent warning messages that ‘fitted probabilities numerically 0 or 1
occurred’ when fitting generalized linear models using
[`glm()`](https://rdrr.io/r/stats/glm.html) or calculating odds ratios
using
[`odds_ratio()`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md);
see examples and Venables & Ripley (2002, pp. 197–8).

`as_binom_contingency()` attempts to coerce an object to
[`class`](https://rdrr.io/r/base/class.html) `"binom_contingency"`. If
`.pn` or `.qn` arguments are not provided, these will be assumed to be
columns `"pn"` and `"qn"` respectively.

## Note

Confidence intervals are calculated using
[`prop.test()`](https://rdrr.io/r/stats/prop.test.html), and are based
on Wilson's score method *without* continuity correction (Newcombe,
1998).

## References

Confidence interval from R's
[`prop.test()`](https://rdrr.io/r/stats/prop.test.html) differs from
hand calculation and result from SAS. [Stack
Exchange](https://stats.stackexchange.com/questions/183225/confidence-interval-from-rs-prop-test-differs-from-hand-calculation-and-resul/183238#183238).

Newcombe R.G. (1998). Two-Sided Confidence Intervals for the Single
Proportion: Comparison of Seven Methods. *Statistics in Medicine*,
**17**, 857-872.
[](https://doi.org/10.1002/(SICI)1097-0258(19980430)17:8%3C857::AID-SIM777%3E3.0.CO;2-E)
[doi:10.1002/(SICI)1097-0258(19980430)17:8\<857::AID-SIM777\>3.0.CO;2-E](https://doi.org/10.1002/%28SICI%291097-0258%2819980430%2917%3A8%3C857%3A%3AAID-SIM777%3E3.0.CO%3B2-E)
.

Venables, W.N. and Ripley, B.D. (2002) *Modern Applied Statistics with
S.* New York: Springer.
[](https://doi.org/10.1007/978-0-387-21706-2)[doi:10.1007/978-0-387-21706-2](https://doi.org/10.1007/978-0-387-21706-2)
.

Yates' continuity correction in confidence interval returned by
`prop.test`. [Stack
Exchange](https://stats.stackexchange.com/questions/5206/yates-continuity-correction-in-confidence-interval-returned-by-prop-test).

## See also

[`drop_zero()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md),
[`glm()`](https://rdrr.io/r/stats/glm.html),
[`odds_ratio()`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md)
and [`prop.test()`](https://rdrr.io/r/stats/prop.test.html);
[`Print_Methods`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
for S3 method for printing objects of class `"binom_contingency"`.

Other contingency_table:
[`contingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md),
[`expl_fcts()`](https://mark-eis.github.io/ParaAnita/reference/expl_fcts.md)

## Examples

``` r
## Bernoulli data with a single explanatory variable
(d <- bernoulli_data())
#> ___________________________
#> Simulated Bernoulli Data: -
#> 
#> # A tibble: 330 × 2
#>    iv       dv
#>  * <fct> <int>
#>  1 a         0
#>  2 a         1
#>  3 a         1
#>  4 a         1
#>  5 a         0
#>  6 a         1
#>  7 a         1
#>  8 a         0
#>  9 a         0
#> 10 a         1
#> # ℹ 320 more rows

d |> binom_contingency(dv)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        28    38
#> 2 b        26    40
#> 3 c        19    47
#> 4 d        18    48
#> 5 e         8    58

d |> binom_contingency(dv, .propci = TRUE)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 7
#>   iv       pn    qn     n     p  lower upper
#> * <fct> <int> <int> <int> <dbl>  <dbl> <dbl>
#> 1 a        28    38    66 0.424 0.312  0.544
#> 2 b        26    40    66 0.394 0.285  0.515
#> 3 c        19    47    66 0.288 0.193  0.406
#> 4 d        18    48    66 0.273 0.180  0.390
#> 5 e         8    58    66 0.121 0.0627 0.221
#>  Confidence level 0.95 

## Use .data pronoun for more informative error messages
d |> binom_contingency(.data$dv)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        28    38
#> 2 b        26    40
#> 3 c        19    47
#> 4 d        18    48
#> 5 e         8    58

try(d |> binom_contingency(dx))
#> Error : object 'dx' not found

try(d |> binom_contingency(.data$dx))
#> Error in .data$dx : Column `dx` not found in `.data`.

## NB this section is intended to be pasted in, rather than run by example()
if (FALSE) { # \dontrun{
    oldopt <- options(warn = 0, nwarnings = 50)

    ## Bernoulli data with identical responses for
    ##   the last level of the explanatory variable
    d <- bernoulli_data(probs = seq(0.4, 0, length.out = 5))
    d |> binom_contingency(dv)

    ## Elicits mutiple warnings in glm.fit()
    ##   'fitted probabilities numerically 0 or 1 occurred'
    d |> binom_contingency(dv) |>
        glm(cbind(pn, qn) ~ iv, binomial, data = _) |>
        confint()
    summary(warnings())

    ##  Argument .drop_zero = TRUE in binom_contingency()
    ##    prevents these warnings
    d |> binom_contingency(dv, .drop_zero = TRUE)

    d |> binom_contingency(dv, .drop_zero = TRUE) |>
        glm(cbind(pn, qn) ~ iv, binomial, data = _) |>
        confint()

    options(oldopt)
} # }

## Bernoulli data with multiple explanatory variables
(d <- list(
    iv2 = list(i = c("a", "c", "e", "g"), j = c("b", "d", "f", "h")),
    iv3 = list(k = c("a", "b", "c", "d"), l = c("e", "f", "g", "h")),
    iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
) |> add_grps(bernoulli_data(levels = 8), iv, .key = _))
#> ___________________________
#> Simulated Bernoulli Data: -
#> 
#> # A tibble: 528 × 5
#>    iv    iv2   iv3   iv4      dv
#>    <fct> <fct> <fct> <fct> <int>
#>  1 a     i     k     k         1
#>  2 a     i     k     k         1
#>  3 a     i     k     k         0
#>  4 a     i     k     k         0
#>  5 a     i     k     k         0
#>  6 a     i     k     k         0
#>  7 a     i     k     k         0
#>  8 a     i     k     k         1
#>  9 a     i     k     k         0
#> 10 a     i     k     k         0
#> # ℹ 518 more rows

d |> binom_contingency(dv)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     i     k     k        24    42
#> 2 b     j     k     k        28    38
#> 3 c     i     k     l        26    40
#> 4 d     j     k     l        25    41
#> 5 e     i     l     m        18    48
#> 6 f     j     l     m        12    54
#> 7 g     i     l     g         8    58
#> 8 h     j     l     h         7    59

d |> binom_contingency(dv, iv, iv3)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 4
#>   iv    iv3      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 a     k        24    42
#> 2 b     k        28    38
#> 3 c     k        26    40
#> 4 d     k        25    41
#> 5 e     l        18    48
#> 6 f     l        12    54
#> 7 g     l         8    58
#> 8 h     l         7    59

d |> binom_contingency(dv, !c(iv2, iv4))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 4
#>   iv    iv3      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 a     k        24    42
#> 2 b     k        28    38
#> 3 c     k        26    40
#> 4 d     k        25    41
#> 5 e     l        18    48
#> 6 f     l        12    54
#> 7 g     l         8    58
#> 8 h     l         7    59

d |> binom_contingency(dv, !!!expl_fcts(d))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     i     k     k        24    42
#> 2 b     j     k     k        28    38
#> 3 c     i     k     l        26    40
#> 4 d     j     k     l        25    41
#> 5 e     i     l     m        18    48
#> 6 f     j     l     m        12    54
#> 7 g     i     l     g         8    58
#> 8 h     j     l     h         7    59

d |> binom_contingency(dv, .propci = TRUE)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 10
#>   iv    iv2   iv3   iv4      pn    qn     n     p  lower upper
#> * <fct> <fct> <fct> <fct> <int> <int> <int> <dbl>  <dbl> <dbl>
#> 1 a     i     k     k        24    42    66 0.364 0.258  0.484
#> 2 b     j     k     k        28    38    66 0.424 0.312  0.544
#> 3 c     i     k     l        26    40    66 0.394 0.285  0.515
#> 4 d     j     k     l        25    41    66 0.379 0.271  0.499
#> 5 e     i     l     m        18    48    66 0.273 0.180  0.390
#> 6 f     j     l     m        12    54    66 0.182 0.107  0.291
#> 7 g     i     l     g         8    58    66 0.121 0.0627 0.221
#> 8 h     j     l     h         7    59    66 0.106 0.0523 0.203
#>  Confidence level 0.95 

d |> binom_contingency(dv, .drop_zero = TRUE)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     i     k     k        24    42
#> 2 b     j     k     k        28    38
#> 3 c     i     k     l        26    40
#> 4 d     j     k     l        25    41
#> 5 e     i     l     m        18    48
#> 6 f     j     l     m        12    54
#> 7 g     i     l     g         8    58
#> 8 h     j     l     h         7    59

d |>
   binom_contingency(dv, iv2, iv3, .drop_zero = TRUE) |>
   glm(cbind(pn, qn) ~ ., binomial, data = _) |>
   summary()
#> 
#> Call:
#> glm(formula = cbind(pn, qn) ~ ., family = binomial, data = binom_contingency(d, 
#>     dv, iv2, iv3, .drop_zero = TRUE))
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.4069     0.1605  -2.536   0.0112 *  
#> iv2j         -0.0799     0.1999  -0.400   0.6894    
#> iv3l         -1.1361     0.2067  -5.496 3.88e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 33.6983  on 3  degrees of freedom
#> Residual deviance:  1.3006  on 1  degrees of freedom
#> AIC: 27.397
#> 
#> Number of Fisher Scoring iterations: 3
#> 

d |>
   binom_contingency(dv, iv2, iv3, .drop_zero = TRUE) |>
   glm(cbind(pn, qn) ~ ., binomial, data = _) |>
   odds_ratio()
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ ., family = binomial, data = binom_contingency(d, dv, iv2, iv3, .drop_zero = TRUE))
#> 
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 3 × 7
#>   parameter   estimate    se  p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>  <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)  -0.407  0.160 0.0112      1          NA         NA     *    
#> 2 iv2j         -0.0799 0.200 0.689       0.923       0.623      1.37  NS   
#> 3 iv3l         -1.14   0.207 0           0.321       0.213      0.479 ***  

## Use {dplyr} selection helpers e.g., last_col(), num_range() and starts_with()
d |> binom_contingency(dv, last_col(1L))  ## Offset of 1L used, since last column of d is dv
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv4      pn    qn
#> * <fct> <int> <int>
#> 1 k        52    80
#> 2 l        51    81
#> 3 m        30   102
#> 4 g         8    58
#> 5 h         7    59

d |> binom_contingency(dv, !last_col(1L))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 5
#>   iv    iv2   iv3      pn    qn
#> * <fct> <fct> <fct> <int> <int>
#> 1 a     i     k        24    42
#> 2 b     j     k        28    38
#> 3 c     i     k        26    40
#> 4 d     j     k        25    41
#> 5 e     i     l        18    48
#> 6 f     j     l        12    54
#> 7 g     i     l         8    58
#> 8 h     j     l         7    59

d |> binom_contingency(dv, num_range("iv", 2:3))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 4 × 4
#>   iv2   iv3      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 i     k        50    82
#> 2 j     k        53    79
#> 3 i     l        26   106
#> 4 j     l        19   113

d |> binom_contingency(dv, !num_range("iv", 2:3))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 4
#>   iv    iv4      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 a     k        24    42
#> 2 b     k        28    38
#> 3 c     l        26    40
#> 4 d     l        25    41
#> 5 e     m        18    48
#> 6 f     m        12    54
#> 7 g     g         8    58
#> 8 h     h         7    59

d |> binom_contingency(dv, starts_with("iv"))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 8 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     i     k     k        24    42
#> 2 b     j     k     k        28    38
#> 3 c     i     k     l        26    40
#> 4 d     j     k     l        25    41
#> 5 e     i     l     m        18    48
#> 6 f     j     l     m        12    54
#> 7 g     i     l     g         8    58
#> 8 h     j     l     h         7    59

d |> binom_contingency(dv, !starts_with("iv")) ## Here, negation excludes all explanatory factors
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 1 × 2
#>      pn    qn
#> * <int> <int>
#> 1   148   380

## as_binom_contingency() 
(d <- data.frame(
        iv = letters[1:5],
        success = c(34, 31, 16, 0, 10),
        failure = c(32, 35, 50, 66, 56)
    ))
#>   iv success failure
#> 1  a      34      32
#> 2  b      31      35
#> 3  c      16      50
#> 4  d       0      66
#> 5  e      10      56

d |> as_binom_contingency(.pn = success, .qn = failure)
#> Coercing `.pn` and/or `.qn` to integer
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <chr> <int> <int>
#> 1 a        34    32
#> 2 b        31    35
#> 3 c        16    50
#> 4 d         0    66
#> 5 e        10    56

d |> as_binom_contingency(.pn = success, .qn = failure, .drop_zero = TRUE)
#> Coercing `.pn` and/or `.qn` to integer
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 4 × 3
#>   iv       pn    qn
#> * <chr> <int> <int>
#> 1 a        34    32
#> 2 b        31    35
#> 3 c        16    50
#> 4 e        10    56

(d <- binom_data())
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        35    31
#> 2 b        26    40
#> 3 c        17    49
#> 4 d        17    49
#> 5 e        10    56

d |> as_binom_contingency()
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        35    31
#> 2 b        26    40
#> 3 c        17    49
#> 4 d        17    49
#> 5 e        10    56

d |> as_binom_contingency(.propci = TRUE)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 7
#>   iv       pn    qn     n     p  lower upper
#> * <fct> <int> <int> <int> <dbl>  <dbl> <dbl>
#> 1 a        35    31    66 0.530 0.412  0.646
#> 2 b        26    40    66 0.394 0.285  0.515
#> 3 c        17    49    66 0.258 0.167  0.374
#> 4 d        17    49    66 0.258 0.167  0.374
#> 5 e        10    56    66 0.152 0.0844 0.257
#>  Confidence level 0.95 

rm(d)
```

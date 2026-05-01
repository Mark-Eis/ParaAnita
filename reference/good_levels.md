# Levels of Independent Variable where a Bernoulli Dependent Variable is Neither All Success Nor All Failure

`good_levels()` identifies `levels` of an independent variable for which
values of a Bernoulli dependent variable are neither all one (success)
nor all zero (failure).

`drop_zero()` drops data with `levels` of an independent variable for
which values of a Bernoulli dependent variable are either all one
(success) or all zero (failure).

## Usage

``` r
good_levels(object, ...)

# S3 method for class 'data.frame'
good_levels(object, .dep_var, .ind_var, ...)

# S3 method for class 'binom_contingency'
good_levels(object, .ind_var, ...)

drop_zero(object, ...)

# S3 method for class 'data.frame'
drop_zero(object, .dep_var, .ind_var, ...)

# S3 method for class 'binom_contingency'
drop_zero(object, ...)

drop_null(object, ...)
```

## Arguments

- object:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)),
  or an object of class `"binom_contingency"`.

- ...:

  further arguments passed to or from other methods.

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of a Bernoulli dependent variable that should be `numeric`
  with values of `0` and `1`.

- .ind_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the independent variable, which may be a `factor`, or a
  `character vector`.

## Value

- `good_levels()`:

  returns a `character vector` comprising `levels` of `.ind_var` for
  which the corresponding values of `.dep_var` are neither all one
  (success) nor all zero (failure)

- `drop_zero()`:

  returns an object of the same
  [`class`](https://rdrr.io/r/base/class.html) as that provided by
  argument `object`: either a data frame (or a data frame extension
  e.g., a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html))
  comprising only rows with levels of the independent variable for which
  values of the Bernoulli dependent variable are neither all zero nor
  all one; or a
  [`"binom_contingency"`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
  object excluding any rows for which values of the Bernoulli dependent
  variable are either all one (success) or all zero (failure).

## Details

For a Bernoulli trial dataset with a numeric dependent variable coded as
`0` or `1`, the generic function `good_levels()` identifies
[`levels`](https://rdrr.io/r/base/levels.html) of an independent
variable for which values of the dependent variable are neither all zero
nor all one i.e., \\0 \< p \< 1\\.

For a similar Bernoulli trial dataset, the generic function
`drop_zero()` drops all rows of data other than those with `levels` of
the independent variable identified by `good_levels()`. Unused factor
levels are dropped from the independent variable and from any other
factors in the data.

The `drop_zero()` S3 method for objects of class `"binom_contingency"`
returns a binomial contingency table equivalent to the original having
been created using
[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
with argument `.drop_zero = TRUE`.

`drop_null()` is deprecated, please use `drop_zero()`.

## Note

Dropping [`levels`](https://rdrr.io/r/base/levels.html) of explanatory
factors for which values of a Bernoulli dependent variable are either
all zero or all one, prevents warning messages that ‘fitted
probabilities numerically 0 or 1 occurred’ when fitting generalized
linear models using [`glm()`](https://rdrr.io/r/stats/glm.html) or
calculating odds ratios using
[`odds_ratio()`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md);
see
[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md).

## See also

[`binom_contingency`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
and [`levels`](https://rdrr.io/r/base/levels.html).

Other levels_data:
[`levels_data()`](https://mark-eis.github.io/ParaAnita/reference/levels_data.md)

## Examples

``` r
(d_bern <- bernoulli_data(probs = c(0.8, 0.4, 0, 0.3, 0.6 )))
#> ___________________________
#> Simulated Bernoulli Data: -
#> 
#> # A tibble: 330 × 2
#>    iv       dv
#>  * <fct> <int>
#>  1 a         1
#>  2 a         0
#>  3 a         1
#>  4 a         1
#>  5 a         0
#>  6 a         1
#>  7 a         1
#>  8 a         1
#>  9 a         1
#> 10 a         0
#> # ℹ 320 more rows
d_bern |> levels_data()
#> $iv
#> [1] "a" "b" "c" "d" "e"
#> 

## S3 methods for class 'data.frame' 
d_bern |> good_levels(dv, iv)
#> [1] "a" "b" "d" "e"
d_bern |> drop_zero(dv, iv)
#> ___________________________
#> Simulated Bernoulli Data: -
#> 
#> # A tibble: 264 × 2
#>    iv       dv
#>    <fct> <int>
#>  1 a         1
#>  2 a         0
#>  3 a         1
#>  4 a         1
#>  5 a         0
#>  6 a         1
#>  7 a         1
#>  8 a         1
#>  9 a         1
#> 10 a         0
#> # ℹ 254 more rows
d_bern |> drop_zero(dv, iv) |> levels_data()
#> $iv
#> [1] "a" "b" "d" "e"
#> 

(d_bin <- d_bern |> binom_contingency(dv, iv))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        50    16
#> 2 b        24    42
#> 3 c         0    66
#> 4 d        20    46
#> 5 e        50    16

## S3 methods for class 'binom_contingency' 
d_bin |> good_levels(iv)
#> [1] "a" "b" "d" "e"
d_bin |> drop_zero()
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 4 × 3
#>   iv       pn    qn
#>   <fct> <int> <int>
#> 1 a        50    16
#> 2 b        24    42
#> 3 d        20    46
#> 4 e        50    16

## Results identical whether drop_zero() used
## before or after binom_contingency()
identical(
  d_bern |> drop_zero(dv, iv) |> binom_contingency(dv, iv),
  d_bin |> drop_zero()
)
#> [1] TRUE

## Results identical whether drop_zero() used
## during or after binom_contingency()
identical(
  d_bern |> binom_contingency(dv, iv, .drop_zero = TRUE),
  d_bin |> drop_zero()
)
#> [1] TRUE

rm(d_bern, d_bin)
```

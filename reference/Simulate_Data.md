# Simulated Bernoulli and Binomial Proportion Data

`bernoulli_data()` creates a simulated univariate Bernoulli data set
having a dependent variable `dv` with values of `0` and `1`, and an
independent variable `iv` with levels represented by lower case letters.

`binom_data()` creates a simulated univariate binomial proportion data
set having a variable `pn` representing the number of successes, a
variable `qn` representing the number of failures, and an independent
variable `iv` with levels represented by lower case letters.

## Usage

``` r
bernoulli_data(
  levels = 5,
  length = 66,
  probs = seq(0.5, 0.1, length.out = levels)
)

binom_data(
  levels = 5,
  length = 66L,
  probs = seq(0.5, 0.1, length.out = levels)
)
```

## Arguments

- levels:

  numeric, the desired number of levels of the independent variable
  `iv`; default `5`.

- length:

  numeric, the desired number of simulated observations per level of the
  independent variable `iv`; default `20`.

- probs:

  a numeric vector of the same length as levels, representing the
  probabilities of success for each corresponding level; default
  `seq(0.5, 0.1, length.out = levels)`.

## Value

An object of class `"announce"` inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
with column `iv` for the independant variable, and for
`bernoulli_data()`, column `dv` representing the dependant variable; and
for `binom_data()`, columns `pn` and `qn` representing the number of
"successes" and "failures", as follows: -

- `iv`:

  a `factor` representing levels of the independant variable.

- `dv`:

  an `integer` representing the value of the dependent variable.

- `pn`:

  an `integer` representing the number of successes.

- `qn`:

  an `integer` representing the number of failures.

## Details

A random sample from a Bernoulli distribution is obtained for each level
of the independent variable `iv`, at the corresponding probability given
in `probs`, using [`rbinom()`](https://rdrr.io/r/stats/Binomial.html)
with `size = 1`. The result is returned as a
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
with two columns, `iv` representing the level of the independent
variable and `dv` representing the simulated data. The result may be
easily converted into (simulated) proportion data and inspected using
[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md),
see examples.

A random sample from a binomial distribution of size `length` is
obtained for each level of the independent variable `iv`, at the
corresponding probability given in `probs`, using
[`rbinom()`](https://rdrr.io/r/stats/Binomial.html) with
`size = levels`.

`bernoulli_data()` and `binom_data()` are used for demonstrating and
testing functions such as
[`contingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md),
[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
and
[`odds_ratio()`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md).

## Note

The default length of 66 is the minimum number of trials with
probability of success of 0.1 for which the overall probability of zero
failures is less than 1 in 1000 i.e., \\(1 - 0.1)^{66} \< 0.001\\.

## See also

[`rbinom()`](https://rdrr.io/r/stats/Binomial.html)

## Examples

``` r
bernoulli_data()
#> ___________________________
#> Simulated Bernoulli Data: -
#> 
#> # A tibble: 330 × 2
#>    iv       dv
#>  * <fct> <int>
#>  1 a         1
#>  2 a         0
#>  3 a         0
#>  4 a         0
#>  5 a         1
#>  6 a         0
#>  7 a         0
#>  8 a         1
#>  9 a         1
#> 10 a         0
#> # ℹ 320 more rows
bernoulli_data() |> binom_contingency(dv, iv)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        30    36
#> 2 b        26    40
#> 3 c        20    46
#> 4 d        11    55
#> 5 e         6    60
bernoulli_data(probs = seq(0.4, 0, length.out = 5)) |> binom_contingency(dv, iv)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        31    35
#> 2 b        19    47
#> 3 c        12    54
#> 4 d         4    62
#> 5 e         0    66

binom_data()
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        36    30
#> 2 b        22    44
#> 3 c        23    43
#> 4 d        10    56
#> 5 e         8    58
binom_data(probs = seq(0.4, 0, length.out = 5))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        26    40
#> 2 b        19    47
#> 3 c        14    52
#> 4 d         8    58
#> 5 e         0    66
```

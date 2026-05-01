# Chi-Squared or Fisher's Exact Test

Test input data using Chi-squared or Fisher's exact test as appropriate.

## Usage

``` r
chsqfish(...)
```

## Arguments

- ...:

  A vector, matrix, data frame or any valid input to `chisq.test`

## Value

A list containing the observed and expected values and the result of
either [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) or
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html), as
appropriate.

## Details

Uses [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) to
calculate expected values and then applies Chi-squared test if all
expected values are 5 or greater, otherwise applies
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`contingency_table`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md).

## Examples

``` r
(t <- bernoulli_data(2, 50, c(0.6, 0.4)) |>
  contingency_table(dv, iv, .rownames = TRUE))
#>    1  0
#> a 37 13
#> b 20 30

t |> chsqfish()
#> $observed
#>    1  0
#> a 37 13
#> b 20 30
#> 
#> $expected
#>      1    0
#> a 28.5 21.5
#> b 28.5 21.5
#> 
#> $test
#> 
#>  Pearson's Chi-squared test with Yates' continuity correction
#> 
#> data:  t
#> X-squared = 10.445, df = 1, p-value = 0.00123
#> 
#> 
#> attr(,"class")
#> [1] "chsqfish"

(t <- bernoulli_data(3, 10, c(0.8, 0.5, 0.2)) |>
  contingency_table(dv, iv, .rownames = TRUE))
#>   1 0
#> a 9 1
#> b 2 8
#> c 3 7

t |> chsqfish()
#> Warning: Chi-squared approximation may be incorrect
#> $observed
#>   1 0
#> a 9 1
#> b 2 8
#> c 3 7
#> 
#> $expected
#>          1        0
#> a 4.666667 5.333333
#> b 4.666667 5.333333
#> c 4.666667 5.333333
#> 
#> $test
#> 
#>  Fisher's Exact Test for Count Data with simulated p-value (based on
#>  2000 replicates)
#> 
#> data:  chsq$observed
#> p-value = 0.002499
#> alternative hypothesis: two.sided
#> 
#> 
#> attr(,"class")
#> [1] "chsqfish"

rm(t)
```

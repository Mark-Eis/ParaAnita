# S3 Print Methods

S3 methods for printing objects of class `"announce"`,
`"binom_contingency"`, `"contingency_table"`, `"odds_ratio"` and
`"summ_anov"`.

## Usage

``` r
# S3 method for class 'announce'
print(x, ...)

# S3 method for class 'binom_contingency'
print(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)

# S3 method for class 'contingency_table'
print(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)

# S3 method for class 'odds_ratio'
print(
  x,
  width = NULL,
  ...,
  n = NULL,
  max_extra_cols = NULL,
  max_footer_lines = NULL,
  digits = 7
)

# S3 method for class 'summ_anov'
print(x, ...)
```

## Arguments

- x:

  an object used to select a method.

- ...:

  further arguments passed to or from other methods.

- width:

  only used when `max.levels` is NULL, see above.

- n:

  Number of rows to show. If `NULL`, the default, will print all rows if
  less than the `print_max`
  [option](https://pillar.r-lib.org/reference/pillar_options.html).
  Otherwise, will print as many rows as specified by the `print_min`
  [option](https://pillar.r-lib.org/reference/pillar_options.html).

- max_extra_cols:

  Number of extra columns to print abbreviated information for, if the
  width is too small for the entire tibble. If `NULL`, the
  `max_extra_cols`
  [option](https://pillar.r-lib.org/reference/pillar_options.html) is
  used. The previously defined `n_extra` argument is soft-deprecated.

- max_footer_lines:

  Maximum number of footer lines. If `NULL`, the `max_footer_lines`
  [option](https://pillar.r-lib.org/reference/pillar_options.html) is
  used.

- digits:

  `integer` indicating the maximum number of decimal places for
  p-values, see [`round()`](https://rdrr.io/r/base/Round.html); default
  7.

## Value

The argument `x`.

## Details

These print methods return their argument `x` invisibly, via
[`invisible()`](https://rdrr.io/r/base/invisible.html).

Notwithstanding that `print.odds_ratio()` rounds p-values to a
*maximimum* number of decimal places as specified by the `digits`
argument, p-values will be printed to show no more than three
significant figures.

## See also

[`print()`](https://rdrr.io/r/base/print.html),
[`print.tbl()`](https://tibble.tidyverse.org/reference/formatting.html),
[`binom_contingency`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md),
[`contingency_table`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md),
[`odds_ratio`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md)
and
[`summ_anov`](https://mark-eis.github.io/ParaAnita/reference/summanov.md).

Other print:
[`announce()`](https://mark-eis.github.io/ParaAnita/reference/announce.md),
[`lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md),
[`print_all()`](https://mark-eis.github.io/ParaAnita/reference/print_all.md)

## Examples

``` r

## print.announce() — print an 'announce' object
announce("x", lead = "Lorem ipsum dolor sit amet")
#> _____________________________
#> Lorem ipsum dolor sit amet: -
#> 
#> [1] "x"

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
#>  4 a         0
#>  5 a         0
#>  6 a         0
#>  7 a         0
#>  8 a         0
#>  9 a         1
#> 10 a         1
#> # ℹ 320 more rows

## print.binom_contingency() — print a 'binom_contingency' object
d |> binom_contingency(dv)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        29    37
#> 2 b        25    41
#> 3 c        27    39
#> 4 d        16    50
#> 5 e         7    59

(d2 <- tibble(
    iv = letters[1:4] |> sample(10, replace = TRUE) |> as.factor(),
    dv = c("Success", "Fail", "Borderline")  |> sample(10, replace = TRUE)
  ))
#> # A tibble: 10 × 2
#>    iv    dv        
#>    <fct> <chr>     
#>  1 a     Borderline
#>  2 a     Success   
#>  3 a     Borderline
#>  4 b     Success   
#>  5 c     Success   
#>  6 a     Fail      
#>  7 c     Fail      
#>  8 c     Borderline
#>  9 b     Success   
#> 10 d     Fail      

## print.contingency_table() — print a 'contingency_table' object
d2 |> contingency_table(dv)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 4 × 4
#>   iv    Borderline Success  Fail
#> * <fct>      <int>   <int> <int>
#> 1 a              2       1     1
#> 2 b              0       2     0
#> 3 c              1       1     1
#> 4 d              0       0     1

## print.odds_ratio() — print an 'odds_ratio' object
d |> odds_ratio(.dep_var = dv, .ind_var = iv)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -0.244 0.248 0.326          1         NA          NA     NS   
#> 2 ivb           -0.251 0.355 0.479          0.778      0.386       1.56  NS   
#> 3 ivc           -0.124 0.352 0.725          0.883      0.441       1.76  NS   
#> 4 ivd           -0.896 0.379 0.0182         0.408      0.191       0.850 *    
#> 5 ive           -1.89  0.470 0.0000599      0.151      0.0562      0.363 ***  

## print.odds_ratio() — print an 'odds_ratio' object with p values to 12 decimal places
d |> odds_ratio(.dep_var = dv, .ind_var = iv) |> print(digits = 12)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -0.244 0.248 0.326          1         NA          NA     NS   
#> 2 ivb           -0.251 0.355 0.479          0.778      0.386       1.56  NS   
#> 3 ivc           -0.124 0.352 0.725          0.883      0.441       1.76  NS   
#> 4 ivd           -0.896 0.379 0.0182         0.408      0.191       0.850 *    
#> 5 ive           -1.89  0.470 0.0000599      0.151      0.0562      0.363 ***  

## print.summanov() — print a 'summanov' object
d |> summanov(dv, iv)
#> _______________________________________
#> GLM Summary and Analysis of Deviance: -
#> 
#> $iv
#> ______________
#> GLM Summary: -
#> 
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!sym(x)), family = .family, 
#>     data = data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.2436     0.2480  -0.982   0.3260    
#> ivb          -0.2511     0.3548  -0.708   0.4792    
#> ivc          -0.1241     0.3524  -0.352   0.7247    
#> ivd          -0.8958     0.3795  -2.361   0.0182 *  
#> ive          -1.8880     0.4704  -4.013 5.99e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 411.29  on 329  degrees of freedom
#> Residual deviance: 385.15  on 325  degrees of freedom
#> AIC: 395.15
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> ____________
#> GLM Anova: -
#> 
#> Analysis of Deviance Table
#> 
#> Model: binomial, link: logit
#> 
#> Response: dv
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                   329     411.29              
#> iv    4   26.132       325     385.15 2.976e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 

rm(d, d2)
```

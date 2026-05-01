# Odds Ratios, Standard Errors, Confidence Intervals and P-Values for Binomial GLMs

`odds_ratio()` calculates odds ratios and their profiled confidence
intervals for GLMs and outputs these together with the estimates of the
regression coefficients, their standard errors and probabilities.

## Usage

``` r
odds_ratio(object, ...)

# S3 method for class 'binom_contingency'
odds_ratio(
  object,
  .ind_var,
  ...,
  .level = 0.95,
  .print_call = FALSE,
  .stat = FALSE,
  .print_contr = FALSE
)

# S3 method for class 'data.frame'
odds_ratio(
  object,
  .dep_var,
  .ind_var,
  ...,
  .level = 0.95,
  .print_call = FALSE,
  .stat = FALSE,
  .print_contr = FALSE
)

# S3 method for class 'formula'
odds_ratio(
  object,
  ...,
  .family = binomial,
  .data,
  .level = 0.95,
  .print_call = FALSE,
  .stat = FALSE,
  .print_contr = FALSE
)

# S3 method for class 'glm'
odds_ratio(
  object,
  ...,
  .level = 0.95,
  .print_call = TRUE,
  .stat = FALSE,
  .print_contr = FALSE
)

# S3 method for class 'odds_ratio'
formula(x, ...)
```

## Arguments

- object:

  an object from which the odds ratios are to be calculated, which may
  be a
  [`binom_contingency`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
  table, a [`data frame`](https://rdrr.io/r/base/data.frame.html) (or a
  data frame extension e.g., a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)),
  a [`formula`](https://rdrr.io/r/stats/formula.html) or a
  [`glm`](https://rdrr.io/r/stats/glm.html).

- ...:

  further arguments passed to or from other methods.

- .ind_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of an independent variable, which may be either a
  character vector or factor.

- .level:

  the confidence level required; default `0.95`.

- .print_call:

  `logical`, whether or not to print the call for the GLM.

- .stat:

  `logical`, whether or not to print `z` or `t` statistic for the GLM;
  default `FALSE`.

- .print_contr:

  `logical`. If `TRUE`, and `.ind_var` has a contrast attribute set, the
  contrast matrix will be printed; default `FALSE`.

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the response variable in the data, either as a
  `numeric vector` with values of `1` and `0`, representing success or
  failure respectively, or as a two-column `matrix` with the columns
  giving the numbers of successes and failures see
  [`glm()`](https://rdrr.io/r/stats/glm.html).

- .family:

  a description of the error distribution and link function to be used
  in the model. This can be a character string naming a family function,
  a family function or the result of a call to a family function. (See
  [`family`](https://rdrr.io/r/stats/family.html) for details of family
  functions.)

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- x:

  an object of class `"odds_ratio"`.

## Value

An object of classes `"odds_ratio"`, `"announce"`, inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html),
and containing the following columns: -

- parameter:

  The names of the model parameters.

- estimate:

  The estimate of the regression coefficient.

- se:

  The standard error of the estimate.

- z (or t) value:

  Optionally, the value of the z (or t) statistic for the estimate.

- p_val:

  The p-value for the estimate.

- odds_ratio:

  The odds ratio.

- ci:

  The lower and upper confidence intervals for the odds ratio, by
  default at the 2.5% and 97.5% levels.

- sig:

  Stars for statistical significance.

## Details

`odds_ratio()` is a generic function used to calculate odds ratios and
their profiled confidence intervals for univariable GLMs with a single
categorical independent variable, or for multivariable GLMs, and output
these together with the estimates of the regression coefficients, their
standard errors and probabilities. The function invokes particular
[`methods`](https://rdrr.io/r/utils/methods.html) which depend on the
[`class`](https://rdrr.io/r/base/class.html) of the first argument.

The S3 method for objects of class `"formula"` or `"glm"` can be used
with either unvariable GLMs, or with multivariable GLMs to calculate
"adjusted" odds ratios. Currently, the S3 methods for classes
`"data.frame"` and `"binom_contingency"` can only be used with
univariable GLMs.

Optionally, if `print_call = TRUE` the call to
[`glm()`](https://rdrr.io/r/stats/glm.html) may be retrieved and
printed.

If `.print_contr = TRUE` and any `factor` independent variables have a
contrast [`attribute`](https://rdrr.io/r/base/attributes.html) set, the
`contrast matrix` will be printed. Contrasts may be set conveniently for
`factors` in `data` using
[`set_contrasts()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md),
see examples.

Confidence intervals for odds ratios are based on profile likelihood and
calculated using `confint.glm()`. The confidence level may be adjusted
using `.level`, otherwise the default value of `0.95` is used.

## See also

[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md),
`contrast matrix`,
[`contrasts()`](https://rdrr.io/r/stats/contrasts.html),
[`formula`](https://rdrr.io/r/stats/formula.html),
[`glm()`](https://rdrr.io/r/stats/glm.html),
[`set_contrasts()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md),
[`summary.glm()`](https://rdrr.io/r/stats/summary.glm.html);
[`Print_Methods`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
and
[`print_all()`](https://mark-eis.github.io/ParaAnita/reference/print_all.md)
for S3 methods for printing objects of class `"odds_ratio"`.

## Examples

``` r
## Create simulated Bernoulli data
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
#>  5 a         1
#>  6 a         0
#>  7 a         1
#>  8 a         1
#>  9 a         0
#> 10 a         0
#> # ℹ 320 more rows

## Invoking the S3 method for class "data.frame" and using the default
## contrasts from options("contrasts")
## — contrasts not printed 
d |> odds_ratio(dv, iv)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)    0.182 0.247 0.461          1         NA          NA     NS   
#> 2 ivb           -0.426 0.350 0.224          0.653      0.327       1.29  NS   
#> 3 ivc           -0.808 0.358 0.0238         0.446      0.219       0.892 *    
#> 4 ivd           -1.91  0.423 0.0000067      0.149      0.0623      0.331 ***  
#> 5 ive           -1.79  0.413 0.0000141      0.167      0.0716      0.364 ***  

## Using the default contrasts from options("contrasts")
## — adjust confidence level, contrasts are printed 
d |> odds_ratio(dv, iv, .level = 0.99, .print_contr = TRUE)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"0.5%"] [,"99.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)    0.182 0.247 0.461          1         NA          NA     NS   
#> 2 ivb           -0.426 0.350 0.224          0.653      0.262       1.60  NS   
#> 3 ivc           -0.808 0.358 0.0238         0.446      0.174       1.11  *    
#> 4 ivd           -1.91  0.423 0.0000067      0.149      0.0463      0.421 ***  
#> 5 ive           -1.79  0.413 0.0000141      0.167      0.0539      0.462 ***  
#> ____________
#> Contrasts: -
#> 
#> $iv
#> [1] "contr.treatment"
#> 

## Specifying treatment contrasts, with last level as base
## — contrasts not printed
d |> set_contrasts(iv, base =  99L, contr = contr.treatment) |>
    odds_ratio(dv, iv)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -1.61  0.330 0.0000011      1          NA          NA    ***  
#> 2 iva            1.79  0.413 0.0000141      6           2.75       14.0  ***  
#> 3 ivb            1.37  0.413 0.000944       3.92        1.78        9.11 ***  
#> 4 ivc            0.984 0.419 0.0190         2.67        1.20        6.27 *    
#> 5 ivd           -0.113 0.476 0.812          0.893       0.346       2.28 NS   

## Specifying treatment contrasts, with last level as base
## — contrasts printed
d |> set_contrasts(iv, base =  99L, contr = contr.treatment) |>
    odds_ratio(dv, iv, .print_contr = TRUE)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -1.61  0.330 0.0000011      1          NA          NA    ***  
#> 2 iva            1.79  0.413 0.0000141      6           2.75       14.0  ***  
#> 3 ivb            1.37  0.413 0.000944       3.92        1.78        9.11 ***  
#> 4 ivc            0.984 0.419 0.0190         2.67        1.20        6.27 *    
#> 5 ivd           -0.113 0.476 0.812          0.893       0.346       2.28 NS   
#> ____________
#> Contrasts: -
#> 
#> $iv
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 0 0 0 1
#> e 0 0 0 0
#> 

## Helmert contrasts specified
## — contrasts printed
d |> set_contrasts(iv, contr = contr.helmert) |>
    odds_ratio(dv, iv, .print_contr = TRUE)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate     se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl>  <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -0.804 0.129  0              1          NA         NA     ***  
#> 2 iv1           -0.213 0.175  0.224          0.808       0.572      1.14  NS   
#> 3 iv2           -0.198 0.104  0.0565         0.820       0.666      1.00  .    
#> 4 iv3           -0.373 0.0932 0.0000612      0.688       0.566      0.818 ***  
#> 5 iv4           -0.201 0.0716 0.00493        0.818       0.704      0.935 **   
#> ____________
#> Contrasts: -
#> 
#> $iv
#>   [,1] [,2] [,3] [,4]
#> a   -1   -1   -1   -1
#> b    1   -1   -1   -1
#> c    0    2   -1   -1
#> d    0    0    3   -1
#> e    0    0    0    4
#> 

# Set default unordered contrasts in options("contrasts") to Helmert
options("contrasts" =  c(unordered = "contr.helmert", ordered = "contr.poly"))
getOption("contrasts")
#>       unordered         ordered 
#> "contr.helmert"    "contr.poly" 

## Using the default, unordered Helmert contrasts
## — contrasts printed
d |> odds_ratio(dv, iv, .print_contr = TRUE)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate     se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl>  <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -0.804 0.129  0              1          NA         NA     ***  
#> 2 iv1           -0.213 0.175  0.224          0.808       0.572      1.14  NS   
#> 3 iv2           -0.198 0.104  0.0565         0.820       0.666      1.00  .    
#> 4 iv3           -0.373 0.0932 0.0000612      0.688       0.566      0.818 ***  
#> 5 iv4           -0.201 0.0716 0.00493        0.818       0.704      0.935 **   
#> ____________
#> Contrasts: -
#> 
#> $iv
#> [1] "contr.helmert"
#> 

## Specify treatment contrasts
## — contrasts printed
d |> set_contrasts(iv, contr = contr.treatment) |>
    odds_ratio(dv, iv, .print_contr = TRUE)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)    0.182 0.247 0.461          1         NA          NA     NS   
#> 2 ivb           -0.426 0.350 0.224          0.653      0.327       1.29  NS   
#> 3 ivc           -0.808 0.358 0.0238         0.446      0.219       0.892 *    
#> 4 ivd           -1.91  0.423 0.0000067      0.149      0.0623      0.331 ***  
#> 5 ive           -1.79  0.413 0.0000141      0.167      0.0716      0.364 ***  
#> ____________
#> Contrasts: -
#> 
#> $iv
#>   b c d e
#> a 0 0 0 0
#> b 1 0 0 0
#> c 0 1 0 0
#> d 0 0 1 0
#> e 0 0 0 1
#> 

## Restore default contrasts in options("contrasts")
options("contrasts" =  c(unordered = "contr.treatment", ordered = "contr.poly"))
options("contrasts")
#> $contrasts
#>         unordered           ordered 
#> "contr.treatment"      "contr.poly" 
#> 

## Invoking the S3 method for class "binom_contingency" 
d |> binom_contingency(dv, iv) |> odds_ratio(iv)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)    0.182 0.247 0.461          1         NA          NA     NS   
#> 2 ivb           -0.426 0.350 0.224          0.653      0.327       1.29  NS   
#> 3 ivc           -0.808 0.358 0.0238         0.446      0.219       0.892 *    
#> 4 ivd           -1.91  0.423 0.0000067      0.149      0.0623      0.331 ***  
#> 5 ive           -1.79  0.413 0.0000141      0.167      0.0716      0.364 ***  

## Create multivariable glm object and specify treatment contrasts
(d <- list(
    iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
    iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f"))
) |> add_grps(binom_data(levels = 6), iv, .key = _))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 6 × 5
#>   iv    iv2   iv3      pn    qn
#>   <fct> <fct> <fct> <int> <int>
#> 1 a     g     i        33    33
#> 2 b     h     i        30    36
#> 3 c     g     i        24    42
#> 4 d     h     j        16    50
#> 5 e     g     j        14    52
#> 6 f     h     j        10    56

set_contr_treat(d, num_range("iv", 2:3)) <- c(1L, 2L)
get_contr_data(d)
#> $iv
#> NULL
#> 
#> $iv2
#>   h
#> g 0
#> h 1
#> 
#> $iv3
#>   i
#> i 1
#> j 0
#> 

glm1 <- glm(cbind(pn, qn) ~ iv2 + iv3, family = binomial, data = d)

glm1 |> summary()
#> 
#> Call:
#> glm(formula = cbind(pn, qn) ~ iv2 + iv3, family = binomial, data = d)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -1.3861     0.2373  -5.841 5.20e-09 ***
#> iv2h          0.0186     0.2363   0.079    0.937    
#> iv3i          1.1363     0.2411   4.714 2.43e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 30.4695  on 5  degrees of freedom
#> Residual deviance:  4.3948  on 3  degrees of freedom
#> AIC: 36.845
#> 
#> Number of Fisher Scoring iterations: 4
#> 

## Invoking the S3 method for class "glm"
glm1 |> odds_ratio()
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ iv2 + iv3, family = binomial, data = d)
#> 
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 3 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)  -1.39   0.237 0               1         NA          NA    ***  
#> 2 iv2h          0.0186 0.236 0.937           1.02       0.642       1.62 NS   
#> 3 iv3i          1.14   0.241 0.0000024       3.12       1.95        5.04 ***  

glm1 |> odds_ratio(.print_call = FALSE, .stat = TRUE, .print_contr = TRUE)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 3 × 8
#>   parameter   estimate    se `z value`     p_val odds_ratio ci[,"2.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>     <dbl>      <dbl>       <dbl> <fct>
#> 1 (Intercept)  -1.39   0.237   -5.84   0               1         NA     ***  
#> 2 iv2h          0.0186 0.236    0.0787 0.937           1.02       0.642 NS   
#> 3 iv3i          1.14   0.241    4.71   0.0000024       3.12       1.95  ***  
#> # ℹ 1 more variable: ci[2] <dbl>
#> ____________
#> Contrasts: -
#> 
#> $iv2
#>   h
#> g 0
#> h 1
#> 
#> $iv3
#>   i
#> i 1
#> j 0
#> 

## Invoking the S3 formula() method for class "odds_ratio"
glm1 |> odds_ratio(.print_call = FALSE) |> formula()
#> Waiting for profiling to be done...
#> cbind(pn, qn) ~ iv2 + iv3
#> <environment: 0x55e097e6f450>

## Compare S3 method for class "glm" to that for "data.frame"
## — only possible for univariable analyses
(d <- binom_data())
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        33    33
#> 2 b        23    43
#> 3 c        20    46
#> 4 d        11    55
#> 5 e         7    59

glm(cbind(pn, qn) ~ iv, family = binomial, data = d) |>
    odds_ratio()
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ iv, family = binomial, data = d)
#> 
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)    0     0.246 1              1         NA          NA     NS   
#> 2 ivb           -0.626 0.357 0.0795         0.535      0.263       1.07  .    
#> 3 ivc           -0.833 0.364 0.0220         0.435      0.211       0.880 *    
#> 4 ivd           -1.61  0.412 0.0000935      0.2        0.0861      0.437 ***  
#> 5 ive           -2.13  0.469 0.0000056      0.119      0.0440      0.284 ***  

d |> odds_ratio(cbind(pn, qn), iv)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)    0     0.246 1              1         NA          NA     NS   
#> 2 ivb           -0.626 0.357 0.0795         0.535      0.263       1.07  .    
#> 3 ivc           -0.833 0.364 0.0220         0.435      0.211       0.880 *    
#> 4 ivd           -1.61  0.412 0.0000935      0.2        0.0861      0.437 ***  
#> 5 ive           -2.13  0.469 0.0000056      0.119      0.0440      0.284 ***  

## Helmert contrasts given more easily readable names
d |> set_contrasts(iv) <- contr.helmert
helm_names(d$iv) <- c(":", "v")
d |> get_contrasts(iv)
#>   a v b a:b v c a:c v d a:d v e
#> a    -1      -1      -1      -1
#> b     1      -1      -1      -1
#> c     0       2      -1      -1
#> d     0       0       3      -1
#> e     0       0       0       4

## Add separator as last little tweak ;-)
contr_colpfx(d$iv) <- ": "

glm(cbind(pn, qn) ~ iv, family = binomial, data = d) |>
    odds_ratio()
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ iv, family = binomial, data = d)
#> 
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate     se   p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl>  <dbl>   <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -1.04  0.137  0            1          NA         NA     ***  
#> 2 iv: a v b     -0.313 0.178  0.0795       0.731       0.513      1.04  .    
#> 3 iv: a:b v c   -0.173 0.107  0.106        0.841       0.678      1.03  NS   
#> 4 iv: a:c v d   -0.281 0.0906 0.00193      0.755       0.625      0.895 **   
#> 5 iv: a:d v e   -0.273 0.0846 0.00126      0.761       0.634      0.888 **   

d |> odds_ratio(cbind(pn, qn), iv)
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 5 × 7
#>   parameter   estimate     se   p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl>  <dbl>   <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -1.04  0.137  0            1          NA         NA     ***  
#> 2 iv: a v b     -0.313 0.178  0.0795       0.731       0.513      1.04  .    
#> 3 iv: a:b v c   -0.173 0.107  0.106        0.841       0.678      1.03  NS   
#> 4 iv: a:c v d   -0.281 0.0906 0.00193      0.755       0.625      0.895 **   
#> 5 iv: a:d v e   -0.273 0.0846 0.00126      0.761       0.634      0.888 **   

## Printing lengthier output with print_all()
binom_data(26, 100) |>
    odds_ratio(cbind(pn, qn), iv, .print_contr = TRUE) |>
    print_all()
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 26 × 7
#>    parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>    <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#>  1 (Intercept)  -0.0400 0.200 0.841          1         NA          NA     NS   
#>  2 ivb           0.120  0.283 0.671          1.13       0.647       1.97  NS   
#>  3 ivc           0.281  0.284 0.322          1.32       0.760       2.32  NS   
#>  4 ivd           0.0400 0.283 0.888          1.04       0.597       1.81  NS   
#>  5 ive          -0.365  0.286 0.201          0.694      0.395       1.21  NS   
#>  6 ivf          -0.579  0.290 0.0457         0.560      0.316       0.986 *    
#>  7 ivg          -0.760  0.295 0.00987        0.468      0.261       0.829 **   
#>  8 ivh          -0.324  0.285 0.256          0.723      0.412       1.26  NS   
#>  9 ivi          -0.579  0.290 0.0457         0.560      0.316       0.986 *    
#> 10 ivj          -0.324  0.285 0.256          0.723      0.412       1.26  NS   
#> 11 ivk          -1.17   0.311 0.000169       0.311      0.167       0.566 ***  
#> 12 ivl          -0.955  0.301 0.00153        0.385      0.211       0.690 **   
#> 13 ivm          -1.11   0.308 0.000303       0.329      0.178       0.596 ***  
#> 14 ivn          -1.41   0.324 0.0000135      0.244      0.127       0.454 ***  
#> 15 ivo          -0.760  0.295 0.00987        0.468      0.261       0.829 **   
#> 16 ivp          -0.714  0.293 0.0149         0.490      0.274       0.866 *    
#> 17 ivq          -1.11   0.308 0.000303       0.329      0.178       0.596 ***  
#> 18 ivr          -1.11   0.308 0.000303       0.329      0.178       0.596 ***  
#> 19 ivs          -1.48   0.328 0.0000069      0.228      0.118       0.428 ***  
#> 20 ivt          -1.35   0.320 0.0000261      0.260      0.137       0.481 ***  
#> 21 ivu          -1.55   0.333 0.0000035      0.213      0.109       0.403 ***  
#> 22 ivv          -2.16   0.389 0              0.116      0.0514      0.239 ***  
#> 23 ivw          -1.69   0.344 0.0000008      0.184      0.0911      0.353 ***  
#> 24 ivx          -1.95   0.367 0.0000001      0.142      0.0666      0.283 ***  
#> 25 ivy          -1.95   0.367 0.0000001      0.142      0.0666      0.283 ***  
#> 26 ivz          -1.78   0.351 0.0000004      0.169      0.0827      0.330 ***  
#> ____________
#> Contrasts: -
#> 
#> $iv
#> [1] "contr.treatment"
#> 

rm(d, glm1)
```

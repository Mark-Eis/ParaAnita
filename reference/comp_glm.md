# Compare Series of Nested GLMs

Compare a series of nested Bernoulli or binomial GLMs by supplying data,
a dependent variable and a list of terms representing the right-hand
side of each of a series of model formulae.

## Usage

``` r
comp_glm(.data, .dep_var, ..., .family = binomial, .arrange_by = desc(AIC))
```

## Arguments

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the binary dependent variable to be used as the LHS of
  the model formula; should be numeric with values of `0` and `1`, or a
  two-column matrix with the columns giving the numbers of successes and
  failures e.g., `cbind(pn, qn)`.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  the RHS of any number of model formulae to be compared, based on
  independent variables in `.data`.

- .family:

  a description of the error distribution and link function to be used
  in the model. Can be a character string naming a family function, a
  family function or the result of a call to a family function; default
  `binomial`.

- .arrange_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of a column for ordering results. Use
  [`desc`](https://dplyr.tidyverse.org/reference/desc.html) to sort by a
  variable in descending order; default `desc(AIC)`.

## Value

An object of classes `"comp_glm"`, `"announce"`, inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
and hence `"data.frame"`, with columns including the RHS of the model
`formula`, the `glm` object and eight further goodness-of-fit measures
output by
[`glance.glm()`](https://broom.tidymodels.org/reference/glance.glm.html),
as follows: -

- f_rhs:

  The right-hand side of the formula as supplied in the `...` argument.

- .glm:

  A list-column containing the glm model objects.

- AIC:

  Akaike's Information Criterion.

- BIC:

  Bayesian Information Criterion.

- deviance:

  Deviance of the model.

- df.null:

  Degrees of freedom for the null model.

- df.residual:

  Residual degrees of freedom.

- logLik:

  The log-likelihood of the model.

- nobs:

  Number of observations.

- null.deviance:

  Deviance of the null model.

## Details

`comp_glm()` builds the formulas from the dependent variable and the
formula right-hand side, calls
[`glm`](https://rdrr.io/r/stats/glm.html), and saves the resulting
objects of class `"glm"` as a list column in a
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html),
together with model fit information obtained using
[`glance.glm`](https://broom.tidymodels.org/reference/glance.glm.html)
from the [broom](https://broom.tidymodels.org/reference/broom.html)
package. The output may be ordered by a selected column, otherwise by
default in descending order of Akaike's Information Criterion (AIC).

`comp_glm()` may be used conveniently to compare a series of related
binomial GLMs based on different groupings of factors added to `.data`
using
[`add_grps`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md).

## Note

It is the user's responsibility to check models are suitably nested to
ensure meaningful comparisons.

## See also

[`add_grps`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md),
[`formula`](https://rdrr.io/r/stats/formula.html),
[`glance.glm`](https://broom.tidymodels.org/reference/glance.glm.html)
and [`glm`](https://rdrr.io/r/stats/glm.html).

Other comp_glm:
[`anova_tbl()`](https://mark-eis.github.io/ParaAnita/reference/anova_tbl.md),
[`summanov()`](https://mark-eis.github.io/ParaAnita/reference/summanov.md),
[`univ_anova()`](https://mark-eis.github.io/ParaAnita/reference/univ_anova.md)

## Examples

``` r
## Simulate Bernoulli data

(d <- list(
        iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
        iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
        iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
    ) |> add_grps(bernoulli_data(levels = 6), iv, .key = _))
#> ___________________________
#> Simulated Bernoulli Data: -
#> 
#> # A tibble: 396 × 5
#>    iv    iv2   iv3   iv4      dv
#>    <fct> <fct> <fct> <fct> <int>
#>  1 a     g     i     k         0
#>  2 a     g     i     k         0
#>  3 a     g     i     k         1
#>  4 a     g     i     k         0
#>  5 a     g     i     k         1
#>  6 a     g     i     k         1
#>  7 a     g     i     k         0
#>  8 a     g     i     k         0
#>  9 a     g     i     k         1
#> 10 a     g     i     k         1
#> # ℹ 386 more rows

## Models arranged in descending order of AIC by default.
d |> comp_glm(dv, iv, iv2, iv3, iv4)
#> ______________________
#> Compare Nested GLMs: -
#> 
#> # A tibble: 4 × 10
#>   f_rhs .glm       null.deviance df.null logLik   AIC   BIC deviance df.residual
#> * <chr> <named li>         <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int>
#> 1 iv2   <glm>               479.     395  -236.  476.  484.     472.         394
#> 2 iv3   <glm>               479.     395  -229.  461.  469.     457.         394
#> 3 iv4   <glm>               479.     395  -227.  461.  473.     455.         393
#> 4 iv    <glm>               479.     395  -223.  459.  483.     447.         390
#> # ℹ 1 more variable: nobs <int>

## Arrange models by formula right-hand side 
(comps <- d |> comp_glm(dv, iv, iv2, iv3, iv4, .arrange_by = f_rhs))
#> ______________________
#> Compare Nested GLMs: -
#> 
#> # A tibble: 4 × 10
#>   f_rhs .glm       null.deviance df.null logLik   AIC   BIC deviance df.residual
#> * <chr> <named li>         <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int>
#> 1 iv    <glm>               479.     395  -223.  459.  483.     447.         390
#> 2 iv2   <glm>               479.     395  -236.  476.  484.     472.         394
#> 3 iv3   <glm>               479.     395  -229.  461.  469.     457.         394
#> 4 iv4   <glm>               479.     395  -227.  461.  473.     455.         393
#> # ℹ 1 more variable: nobs <int>

## Inspect components of .glm list-column
lapply(comps$.glm, formula)
#> $iv
#> dv ~ iv
#> <environment: 0x55e09fae89f0>
#> 
#> $iv2
#> dv ~ iv2
#> <environment: 0x55e09fa7d070>
#> 
#> $iv3
#> dv ~ iv3
#> <environment: 0x55e09fa172c8>
#> 
#> $iv4
#> dv ~ iv4
#> <environment: 0x55e09f9a20a0>
#> 

lapply(comps$.glm, summary)
#> $iv
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.06062    0.24630   0.246 0.805570    
#> ivb         -0.68633    0.35693  -1.923 0.054493 .  
#> ivc         -0.75377    0.35895  -2.100 0.035733 *  
#> ivd         -1.20006    0.37837  -3.172 0.001516 ** 
#> ive         -1.37281    0.38900  -3.529 0.000417 ***
#> ivf         -2.19225    0.46953  -4.669 3.03e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 478.96  on 395  degrees of freedom
#> Residual deviance: 446.76  on 390  degrees of freedom
#> AIC: 458.76
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv2
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.6035     0.1487  -4.060 4.91e-05 ***
#> iv2h         -0.5917     0.2245  -2.635  0.00841 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 478.96  on 395  degrees of freedom
#> Residual deviance: 471.90  on 394  degrees of freedom
#> AIC: 475.9
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv3
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.4097     0.1451  -2.823  0.00476 ** 
#> iv3j         -1.0608     0.2330  -4.552  5.3e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 478.96  on 395  degrees of freedom
#> Residual deviance: 457.08  on 394  degrees of freedom
#> AIC: 461.08
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv4
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.2744     0.1757  -1.562   0.1183    
#> iv4l         -0.6313     0.2604  -2.424   0.0154 *  
#> iv4m         -1.3906     0.2958  -4.701 2.59e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 478.96  on 395  degrees of freedom
#> Residual deviance: 454.67  on 393  degrees of freedom
#> AIC: 460.67
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 

## Convert to binomial data using binom_contingency()
(d <- d |> binom_contingency(dv))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        34    32
#> 2 b     h     i     k        23    43
#> 3 c     g     i     l        22    44
#> 4 d     h     j     l        16    50
#> 5 e     g     j     m        14    52
#> 6 f     h     j     m         7    59

(comps <- d |> comp_glm(cbind(pn, qn), iv, iv2, iv3, iv4))
#> ______________________
#> Compare Nested GLMs: -
#> 
#> # A tibble: 4 × 10
#>   f_rhs .glm       null.deviance df.null logLik   AIC   BIC deviance df.residual
#> * <chr> <named li>         <dbl>   <int>  <dbl> <dbl> <dbl>    <dbl>       <int>
#> 1 iv2   <glm>               32.2       5  -25.6  55.2  54.8 2.51e+ 1           4
#> 2 iv3   <glm>               32.2       5  -18.2  40.4  39.9 1.03e+ 1           4
#> 3 iv4   <glm>               32.2       5  -17.0  39.9  39.3 7.91e+ 0           3
#> 4 iv    <glm>               32.2       5  -13.0  38.0  36.8 2.26e-14           0
#> # ℹ 1 more variable: nobs <int>

## Inspect components of .glm list-column
lapply(comps$.glm, formula)
#> $iv2
#> cbind(pn, qn) ~ iv2
#> <environment: 0x55e0a29b3148>
#> 
#> $iv3
#> cbind(pn, qn) ~ iv3
#> <environment: 0x55e0a24170d8>
#> 
#> $iv4
#> cbind(pn, qn) ~ iv4
#> <environment: 0x55e0a1bc8080>
#> 
#> $iv
#> cbind(pn, qn) ~ iv
#> <environment: 0x55e0a29019c8>
#> 

lapply(comps$.glm, summary)
#> $iv2
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.6035     0.1487  -4.060 4.91e-05 ***
#> iv2h         -0.5917     0.2245  -2.635  0.00841 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 32.209  on 5  degrees of freedom
#> Residual deviance: 25.148  on 4  degrees of freedom
#> AIC: 55.172
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv3
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.4097     0.1451  -2.823  0.00476 ** 
#> iv3j         -1.0608     0.2330  -4.552  5.3e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 32.209  on 5  degrees of freedom
#> Residual deviance: 10.327  on 4  degrees of freedom
#> AIC: 40.352
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv4
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  -0.2744     0.1757  -1.562   0.1183    
#> iv4l         -0.6313     0.2604  -2.424   0.0154 *  
#> iv4m         -1.3906     0.2958  -4.701 2.59e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 32.2088  on 5  degrees of freedom
#> Residual deviance:  7.9103  on 3  degrees of freedom
#> AIC: 39.935
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv
#> 
#> Call:
#> glm(formula = inject(!!.dep_var ~ !!x), family = .family, data = .data)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.06062    0.24630   0.246 0.805570    
#> ivb         -0.68633    0.35693  -1.923 0.054493 .  
#> ivc         -0.75377    0.35895  -2.100 0.035733 *  
#> ivd         -1.20006    0.37837  -3.172 0.001516 ** 
#> ive         -1.37281    0.38900  -3.529 0.000417 ***
#> ivf         -2.19225    0.46954  -4.669 3.03e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 3.2209e+01  on 5  degrees of freedom
#> Residual deviance: 2.2649e-14  on 0  degrees of freedom
#> AIC: 38.025
#> 
#> Number of Fisher Scoring iterations: 3
#> 
#> 

rm(comps, d)
```

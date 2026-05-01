# List of Summary and Analysis of Deviance Objects for Related Univariable GLMs

`summanov()` provides a list of the summary and analysis of deviance
objects for a series of related univariable GLMs of data with a binary
dependent variable or a (two-column) dependent variable of binomial
proportions.

## Usage

``` r
summanov(data, .dep_var, ..., .family = binomial, .test = "Chisq")
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the binary dependent variable to be used as the LHS of
  the model formula; should be numeric with values of `0` and `1`, or a
  two-column matrix with the columns giving the numbers of successes and
  failures e.g., `cbind(pn, qn)`.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  quoted name(s) of one or more factors or character vectors in `.data`,
  to be included (or excluded) as independent variables in the list of
  GLM analyses.

- .family:

  a description of the error distribution and link function to be used
  in the model. Can be a character string naming a family function, a
  family function or the result of a call to a family function; default
  `binomial`.

- .test:

  a character string, (partially) matching one of `"Chisq"`, `"LRT"`,
  `"Rao"`, `"F"` or `"Cp"`; default `"Chisq"`.

## Value

A [`list`](https://rdrr.io/r/base/list.html) of `summ_anov` objects of
length equal to the number of factors or character vectors selected
using the `...` arguments. A `summ_anov` object is simply a list with
class `"summ_anov"`, comprising the following two elements: -

- summary:

  Summary of the generalised linear model fit given by
  [`summary.glm`](https://rdrr.io/r/stats/summary.glm.html).

- anova:

  Analysis of deviance for the generalised linear model fit given by
  [`anova.glm`](https://rdrr.io/r/stats/anova.glm.html).

## Details

Variables in `.data` to be included (or excluded) as independent
variables in the list of GLM analyses may be selected using the `...`
argument with
the\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax of package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of **selection helpers**.

The structure of the output list may be changed from a list of pairs
into pair of lists conveniently using
[`list_transpose`](https://purrr.tidyverse.org/reference/list_transpose.html).
The univariable GLMs may then be easily compared and likewise the
univariable GLM anovas (analysis of deviance).

[`univ_anova`](https://mark-eis.github.io/ParaAnita/reference/univ_anova.md)
provides a succinct summmary of the univariable analyses of deviance for
all potential categorical independent variables in `data`.
[`anova_tbl`](https://mark-eis.github.io/ParaAnita/reference/anova_tbl.md)
also provides a succinct summmary from the list of anovas.

## See also

[`anova.glm()`](https://rdrr.io/r/stats/anova.glm.html),
[`list_transpose()`](https://purrr.tidyverse.org/reference/list_transpose.html),
[`glm()`](https://rdrr.io/r/stats/glm.html) and
[`summary.glm()`](https://rdrr.io/r/stats/summary.glm.html);
[`Print_Methods`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
for S3 method for printing objects of class `"summ_anov"`.

Other comp_glm:
[`anova_tbl()`](https://mark-eis.github.io/ParaAnita/reference/anova_tbl.md),
[`comp_glm()`](https://mark-eis.github.io/ParaAnita/reference/comp_glm.md),
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
#>  1 a     g     i     k         1
#>  2 a     g     i     k         0
#>  3 a     g     i     k         1
#>  4 a     g     i     k         1
#>  5 a     g     i     k         1
#>  6 a     g     i     k         1
#>  7 a     g     i     k         1
#>  8 a     g     i     k         1
#>  9 a     g     i     k         0
#> 10 a     g     i     k         1
#> # ℹ 386 more rows

## Binary dependent variable
d |> summanov(dv, starts_with("iv"))
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
#> (Intercept)   0.1214     0.2466   0.492 0.622674    
#> ivb          -0.6161     0.3539  -1.741 0.081694 .  
#> ivc          -0.5521     0.3525  -1.566 0.117314    
#> ivd          -0.6161     0.3539  -1.741 0.081694 .  
#> ive          -1.5267     0.3958  -3.858 0.000114 ***
#> ivf          -2.2530     0.4697  -4.797 1.61e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 502.72  on 395  degrees of freedom
#> Residual deviance: 465.05  on 390  degrees of freedom
#> AIC: 477.05
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
#> NULL                   395     502.72              
#> iv    5   37.673       390     465.05 4.389e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv2
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
#> (Intercept)  -0.5162     0.1469  -3.514 0.000441 ***
#> iv2h         -0.3895     0.2150  -1.812 0.070016 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 502.72  on 395  degrees of freedom
#> Residual deviance: 499.42  on 394  degrees of freedom
#> AIC: 503.42
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
#>      Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
#> NULL                   395     502.72           
#> iv2   1   3.3037       394     499.42  0.06913 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv3
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
#> (Intercept)  -0.2642     0.1434  -1.842   0.0654 .  
#> iv3j         -0.9596     0.2221  -4.321 1.55e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 502.72  on 395  degrees of freedom
#> Residual deviance: 483.30  on 394  degrees of freedom
#> AIC: 487.3
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
#> NULL                   395     502.72              
#> iv3   1   19.419       394     483.30 1.049e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv4
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
#> (Intercept)  -0.1823     0.1748  -1.043    0.297    
#> iv4l         -0.2803     0.2500  -1.121    0.262    
#> iv4m         -1.5404     0.2991  -5.150 2.61e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 502.72  on 395  degrees of freedom
#> Residual deviance: 470.30  on 393  degrees of freedom
#> AIC: 476.3
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
#> NULL                   395     502.72              
#> iv4   2   32.424       393     470.30 9.105e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
d |> summanov(dv, starts_with("iv") & !iv2)
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
#> (Intercept)   0.1214     0.2466   0.492 0.622674    
#> ivb          -0.6161     0.3539  -1.741 0.081694 .  
#> ivc          -0.5521     0.3525  -1.566 0.117314    
#> ivd          -0.6161     0.3539  -1.741 0.081694 .  
#> ive          -1.5267     0.3958  -3.858 0.000114 ***
#> ivf          -2.2530     0.4697  -4.797 1.61e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 502.72  on 395  degrees of freedom
#> Residual deviance: 465.05  on 390  degrees of freedom
#> AIC: 477.05
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
#> NULL                   395     502.72              
#> iv    5   37.673       390     465.05 4.389e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv3
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
#> (Intercept)  -0.2642     0.1434  -1.842   0.0654 .  
#> iv3j         -0.9596     0.2221  -4.321 1.55e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 502.72  on 395  degrees of freedom
#> Residual deviance: 483.30  on 394  degrees of freedom
#> AIC: 487.3
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
#> NULL                   395     502.72              
#> iv3   1   19.419       394     483.30 1.049e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv4
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
#> (Intercept)  -0.1823     0.1748  -1.043    0.297    
#> iv4l         -0.2803     0.2500  -1.121    0.262    
#> iv4m         -1.5404     0.2991  -5.150 2.61e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 502.72  on 395  degrees of freedom
#> Residual deviance: 470.30  on 393  degrees of freedom
#> AIC: 476.3
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
#> NULL                   395     502.72              
#> iv4   2   32.424       393     470.30 9.105e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 

## Binomial proportions
(d <- d |> binom_contingency(dv, starts_with("iv")))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        35    31
#> 2 b     h     i     k        25    41
#> 3 c     g     i     l        26    40
#> 4 d     h     j     l        25    41
#> 5 e     g     j     m        13    53
#> 6 f     h     j     m         7    59

(uva <- d |> summanov(cbind(pn, qn), num_range("iv", 2:4)))
#> _______________________________________
#> GLM Summary and Analysis of Deviance: -
#> 
#> $iv2
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
#> (Intercept)  -0.5162     0.1469  -3.514 0.000441 ***
#> iv2h         -0.3895     0.2150  -1.812 0.070016 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 37.673  on 5  degrees of freedom
#> Residual deviance: 34.369  on 4  degrees of freedom
#> AIC: 64.688
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
#> Response: cbind(pn, qn)
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>      Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
#> NULL                     5     37.673           
#> iv2   1   3.3037         4     34.369  0.06913 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv3
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
#> (Intercept)  -0.2642     0.1434  -1.842   0.0654 .  
#> iv3j         -0.9596     0.2221  -4.321 1.55e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 37.673  on 5  degrees of freedom
#> Residual deviance: 18.254  on 4  degrees of freedom
#> AIC: 48.573
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
#> Response: cbind(pn, qn)
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                     5     37.673              
#> iv3   1   19.419         4     18.254 1.049e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv4
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
#> (Intercept)  -0.1823     0.1748  -1.043    0.297    
#> iv4l         -0.2803     0.2500  -1.121    0.262    
#> iv4m         -1.5404     0.2991  -5.150 2.61e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 37.6731  on 5  degrees of freedom
#> Residual deviance:  5.2494  on 3  degrees of freedom
#> AIC: 37.568
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
#> Response: cbind(pn, qn)
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                     5     37.673              
#> iv4   2   32.424         3      5.249 9.105e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 

## Change list of pairs into a pair of lists using {purrr} list_transpose()
list_transpose(uva)$summary
#> $iv2
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
#> (Intercept)  -0.5162     0.1469  -3.514 0.000441 ***
#> iv2h         -0.3895     0.2150  -1.812 0.070016 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 37.673  on 5  degrees of freedom
#> Residual deviance: 34.369  on 4  degrees of freedom
#> AIC: 64.688
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv3
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
#> (Intercept)  -0.2642     0.1434  -1.842   0.0654 .  
#> iv3j         -0.9596     0.2221  -4.321 1.55e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 37.673  on 5  degrees of freedom
#> Residual deviance: 18.254  on 4  degrees of freedom
#> AIC: 48.573
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 
#> $iv4
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
#> (Intercept)  -0.1823     0.1748  -1.043    0.297    
#> iv4l         -0.2803     0.2500  -1.121    0.262    
#> iv4m         -1.5404     0.2991  -5.150 2.61e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 37.6731  on 5  degrees of freedom
#> Residual deviance:  5.2494  on 3  degrees of freedom
#> AIC: 37.568
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> 

list_transpose(uva)$anova
#> $iv2
#> ____________
#> GLM Anova: -
#> 
#> Analysis of Deviance Table
#> 
#> Model: binomial, link: logit
#> 
#> Response: cbind(pn, qn)
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>      Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
#> NULL                     5     37.673           
#> iv2   1   3.3037         4     34.369  0.06913 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv3
#> ____________
#> GLM Anova: -
#> 
#> Analysis of Deviance Table
#> 
#> Model: binomial, link: logit
#> 
#> Response: cbind(pn, qn)
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                     5     37.673              
#> iv3   1   19.419         4     18.254 1.049e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> $iv4
#> ____________
#> GLM Anova: -
#> 
#> Analysis of Deviance Table
#> 
#> Model: binomial, link: logit
#> 
#> Response: cbind(pn, qn)
#> 
#> Terms added sequentially (first to last)
#> 
#> 
#>      Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#> NULL                     5     37.673              
#> iv4   2   32.424         3      5.249 9.105e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 

rm(d, uva)
```

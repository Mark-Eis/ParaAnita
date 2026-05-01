# Create Tibble from List of Anovas

Create a tibble from a list of anovas that compare a model to the null
model or that compare two nested models.

## Usage

``` r
anova_tbl(anova_ls)
```

## Arguments

- anova_ls:

  a [`list`](https://rdrr.io/r/base/list.html) of `anova` objects.

## Value

An object of classes `"anova_tbl"`, `"announce"`, inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html),
showing the table entries for each anova, one per line.

## Details

The anovas to be compared must all be of the same type i.e., all must be
analyses of a single model object or all must be comparisons of two
models. If any of the models has more than one independent variable, the
results may be difficult to interpret and a warning will be given.

`anova_tbl()` can be used to be easily and conveniently compare a list
of anovas obtained with
[`summanov`](https://mark-eis.github.io/ParaAnita/reference/summanov.md)
and
[`list_transpose`](https://purrr.tidyverse.org/reference/list_transpose.html),
see examples.

## See also

[`anova.glm()`](https://rdrr.io/r/stats/anova.glm.html)
[`list_transpose()`](https://purrr.tidyverse.org/reference/list_transpose.html),
and
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html).

Other comp_glm:
[`comp_glm()`](https://mark-eis.github.io/ParaAnita/reference/comp_glm.md),
[`summanov()`](https://mark-eis.github.io/ParaAnita/reference/summanov.md),
[`univ_anova()`](https://mark-eis.github.io/ParaAnita/reference/univ_anova.md)

## Examples

``` r
## Following on from summanov() examples… 
## Simulate Bernoulli data
(d <- list(
    iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
    iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
    iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
) |> add_grps(binom_data(levels = 6), iv, .key = _))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#>   <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        38    28
#> 2 b     h     i     k        27    39
#> 3 c     g     i     l        19    47
#> 4 d     h     j     l        14    52
#> 5 e     g     j     m         9    57
#> 6 f     h     j     m         6    60

## Create list of GLM anovas using summanov()
(alist <- d |> summanov(cbind(pn, qn), starts_with("iv")) |>
    list_transpose() |> _$anova)
#> $iv
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
#> NULL                     5      54.06              
#> iv    5    54.06         0       0.00 2.037e-10 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
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
#> NULL                     5     54.060           
#> iv2   1   4.4865         4     49.573  0.03416 *
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
#> NULL                     5      54.06              
#> iv3   1   38.699         4      15.36 4.944e-10 ***
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
#> NULL                     5     54.060              
#> iv4   2   48.681         3      5.379 2.686e-11 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 

## Tibble from list of GLM anovas 
alist |> anova_tbl()
#> _________________________________________________
#> Analysis of Deviance (Comparing to null model): -
#> 
#> # A tibble: 4 × 9
#>   Name  `Null Df` `Null Dev`    Df Deviance `Resid. Df` `Resid. Dev` `Pr(>Chi)`
#> * <chr>     <int>      <dbl> <int>    <dbl>       <int>        <dbl>      <dbl>
#> 1 iv            5       54.1     5    54.1            0     5.77e-15   2.04e-10
#> 2 iv2           5       54.1     1     4.49           4     4.96e+ 1   3.42e- 2
#> 3 iv3           5       54.1     1    38.7            4     1.54e+ 1   4.94e-10
#> 4 iv4           5       54.1     2    48.7            3     5.38e+ 0   2.69e-11
#> # ℹ 1 more variable: sig <fct>

## Add GLM anova with two independent variables
alist$iv3_iv4 <- glm(cbind(pn, qn) ~ iv3 + iv4, family = binomial, data = d) |>
    anova()

## Tibble from list with multivariable anova invokes warning
try(alist |> anova_tbl())
#> Error in anova_tbl(alist) : 
#>   all(map_lgl(anova_ls, isa, c("announce", "anova", "data.frame"))) is not TRUE

rm(d, alist)
```

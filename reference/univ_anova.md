# Analyses of Deviance Summarising Fits of Univariable GLMs

`univ_anova()` provides a succinct summary of the analyses of deviance
for all univariable GLMs based on each possible categorical independent
variable in a Bernoulli or binomial data set.

## Usage

``` r
univ_anova(
  data,
  .dep_var,
  .family = binomial,
  .test = c("none", "Rao", "LRT", "Chisq", "F")
)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the binary dependent variable, which should be
  `numeric` with values of `0` and `1`, or a two-column matrix with the
  columns giving the numbers of successes and failures e.g.,
  `cbind(pn, qn)`.

- .family:

  a family function; default `"binomial"`. (See
  [`family`](https://rdrr.io/r/stats/family.html) for details of family
  functions.)

- .test:

  a character string, (partially) matching one of `"none"`, `"Chisq"`,
  `"LRT"`, `"Rao"`, `"F"` or `"Cp"`; default `"none"`.

## Value

An object of class `c("univ_anova" "announce")`, inheriting from
`"anova"` and `"data.frame"` as for
[`add1()`](https://rdrr.io/r/stats/add1.html), summarising the
differences between the fitted univariable GLMs and the null model.

## Details

`univ_anova()` uses [`add1()`](https://rdrr.io/r/stats/add1.html) in the
[stats](https://rdrr.io/r/stats/stats-package.html) package to compare
univariable GLMs for each possible categorical independent variable
(i.e., `"factor"` columns) in `.data` with the null model. Other data
types e.g. `"character"` vectors will be ignored and should be converted
to `"factor"` to be included in this analysis.

## See also

[`add1()`](https://rdrr.io/r/stats/add1.html),
[`anova()`](https://rdrr.io/r/stats/anova.html),
[`anova.glm()`](https://rdrr.io/r/stats/anova.glm.html), and
[`glm()`](https://rdrr.io/r/stats/glm.html).

Other comp_glm:
[`anova_tbl()`](https://mark-eis.github.io/ParaAnita/reference/anova_tbl.md),
[`comp_glm()`](https://mark-eis.github.io/ParaAnita/reference/comp_glm.md),
[`summanov()`](https://mark-eis.github.io/ParaAnita/reference/summanov.md)

## Examples

``` r
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
#>  2 a     g     i     k         1
#>  3 a     g     i     k         0
#>  4 a     g     i     k         1
#>  5 a     g     i     k         0
#>  6 a     g     i     k         1
#>  7 a     g     i     k         0
#>  8 a     g     i     k         1
#>  9 a     g     i     k         0
#> 10 a     g     i     k         1
#> # ℹ 386 more rows

d |> univ_anova(dv, .test = "LRT")
#> ___________________________________
#> Univariable Analysis of Deviance: -
#> 
#> Single term additions
#> 
#> Model:
#> dv ~ 1
#>        Df Deviance    AIC    LRT  Pr(>Chi)    
#> <none>      460.10 462.10                     
#> iv      5   420.12 432.12 39.984 1.505e-07 ***
#> iv2     1   459.27 463.27  0.825    0.3637    
#> iv3     1   441.18 445.18 18.924 1.360e-05 ***
#> iv4     2   426.34 432.34 33.759 4.670e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

d |> binom_contingency(dv, starts_with("iv")) |> print() |>
     univ_anova(cbind(pn, qn), .test = "LRT")
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        30    36
#> 2 b     h     i     k        28    38
#> 3 c     g     i     l        14    52
#> 4 d     h     j     l        17    49
#> 5 e     g     j     m        13    53
#> 6 f     h     j     m         4    62
#> ___________________________________
#> Univariable Analysis of Deviance: -
#> 
#> Single term additions
#> 
#> Model:
#> cbind(pn, qn) ~ 1
#>        Df Deviance    AIC    LRT  Pr(>Chi)    
#> <none>      39.984 67.286                     
#> iv      5    0.000 37.302 39.984 1.505e-07 ***
#> iv2     1   39.159 68.461  0.825    0.3637    
#> iv3     1   21.060 50.361 18.924 1.360e-05 ***
#> iv4     2    6.225 37.527 33.759 4.670e-08 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

rm(d)
```

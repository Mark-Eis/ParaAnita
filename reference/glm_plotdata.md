# Collate Data for Plotting Univariable GLM Predictions with Error Bars

`glm_plotdata()` outputs data based on predictions from univariable
general linear models (GLMs) suitably collated for easy creation of
standardised plots with error bars representing confidence intervals or
standard errors.

## Usage

``` r
glm_plotdata(object, ...)

# S3 method for class 'binom_contingency'
glm_plotdata(
  object,
  .ind_var,
  ...,
  .ungroup = NULL,
  conf_level = 0.95,
  type = c("link", "response")
)

# S3 method for class 'data.frame'
glm_plotdata(
  object,
  .dep_var,
  .ind_var,
  ...,
  .ungroup = NULL,
  conf_level = 0.95,
  type = c("link", "response")
)

# S3 method for class 'formula'
glm_plotdata(
  object,
  ...,
  .family = binomial,
  .data,
  .ungroup = NULL,
  conf_level = 0.95,
  type = c("link", "response")
)

# S3 method for class 'glm'
glm_plotdata(object, ..., conf_level = 0.95, type = c("link", "response"))
```

## Arguments

- object:

  an object from which the plot data are to be calculated, which may be
  a
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

- .ungroup:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the column containing the ungrouped levels of
  `.ind_var`, see details; default `NULL`.

- conf_level:

  the confidence level required for the error bars; default `0.95`. If
  `NA`, error bars are standard error.

- type:

  the type of prediction required. The default is on the scale of the
  linear predictors; the alternative `"response"` is on the scale of the
  response variable; default `"link"`.

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

## Value

An object of class `"glm_plotdata"`, `"announce"`, inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html),
with values on the linear predictor or response scale (depending on
`type`) in columns as follows: -

- level:

  Level of the independent variable.

- grouped:

  Grouped levels of the independent variable (or `NA` if ungrouped).

- n:

  Number of observations.

- obs:

  Observed values.

- pred:

  Values predicted by the model.

- lower:

  Lower extent of error bar.

- upper:

  Upper extent of error bar.

It also has attributes `"conf_level"`, signifying the confidence level,
`"ind_var"`, the name of the independent variable, and `"type"` (see
argument `type`).

## Details

This function works with univariable binomial GLMs having a `numeric`
dependent variable of ones and zeros representing numbers of successes
and failures, or a two-column `matrix` with the columns giving the
numbers of successes and failures, see
[`glm()`](https://rdrr.io/r/stats/glm.html), and an independent variable
with multiple levels. Its output may be plotted conveniently using
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) in
package
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html);
[ParaAnita](https://mark-eis.github.io/ParaAnita/reference/ParaAnita-package.md)
provides a suitable S3 method
[`ggplot.glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/plot_model.md)
for this purpose.

`glm_plotdata()` allows exploration of proposed groupings of the levels
of the independent variable, such as obtained using
[`add_grps()`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md)
or
[`fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html),
and will include both the grouped and ungrouped levels in its output. In
such cases, `.ind_var` should contain the groupings and the `.ungroup`
argument should name a column in `object`'s data containing the
ungrouped levels, see examples. The grouped levels are used as the
independent variable in the GLM and are shown within the output object
in the column `grouped` while the ungrouped levels are shown in the
column `level`. If the `.ungroup` is `NULL` (the default), levels of
`.ind_var` will appear in the column `level` and the `grouped` column in
the output will contain [`NA`](https://rdrr.io/r/base/NA.html).

If `conf_level` is `0.95` (the default) or a similar value, the `lower`
and `upper` columns in the output delimit the prediction intervals at
that confidence level. If `conf_level` is
[`NA`](https://rdrr.io/r/base/NA.html), then `lower` and `upper` are the
model predictions ±standard error.

If `type = "link"`, then the linear predictors and their confidence
intervals or ±standard errors are obtained. If `type = "response"`, then
the linear predictors and their confidence intervals or ±standard errors
are transformed back to the response scale using the link inverse
function.

## Note

Confidence intervals are calculated from the standard errors of the
parameter estimates using the quantiles of the t distribution with *n -
1* degrees of freedom, at the probability given by `conf_level`. These
confidence intervals are generally more conservative i.e., a little
wider, than those obtained by "profiling" (e.g., using `confint.glm`).
If the `conf_level` argument is [`NA`](https://rdrr.io/r/base/NA.html),
standard error is shown rather than a confidence interval.

If `object` is a
[`binom_contingency`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
table it should be "univariable" i.e., having only one row for each
level of `.ind_var`, otherwise the output of `glm_plotdata()` will be
undefined.

## See also

[`add_grps`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md),
[`binom_contingency`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md),
[`formula`](https://rdrr.io/r/stats/formula.html),
[`glm`](https://rdrr.io/r/stats/glm.html), and
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html).

Other plot_model:
[`Plot_Model`](https://mark-eis.github.io/ParaAnita/reference/plot_model.md),
[`glm_plotlist()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotlist.md),
[`var_labs()`](https://mark-eis.github.io/ParaAnita/reference/var_labs.md)

## Examples

``` r
(d <- binom_data())
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        32    34
#> 2 b        33    33
#> 3 c        25    41
#> 4 d        13    53
#> 5 e         6    60

## ___________________________________________________
## Ungrouped data, 95% Confidence interval (default)

## On linear predictor scale (default)
d |> glm_plotdata(cbind(pn, qn), iv)
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n     obs    pred  lower    upper
#> * <fct> <fct>   <int>   <dbl>   <dbl>  <dbl>    <dbl>
#> 1 a     NA         66 -0.0606 -0.0606 -0.545  0.424  
#> 2 b     NA         66  0       0      -0.484  0.484  
#> 3 c     NA         66 -0.495  -0.495  -0.994  0.00449
#> 4 d     NA         66 -1.41   -1.41   -2.01  -0.796  
#> 5 e     NA         66 -2.30   -2.30   -3.14  -1.46   

## On response scale
d |> glm_plotdata(cbind(pn, qn), iv, type = "response")
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n    obs   pred  lower upper
#> * <fct> <fct>   <int>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 a     NA         66 0.485  0.485  0.367  0.604
#> 2 b     NA         66 0.5    0.5    0.381  0.619
#> 3 c     NA         66 0.379  0.379  0.270  0.501
#> 4 d     NA         66 0.197  0.197  0.118  0.311
#> 5 e     NA         66 0.0909 0.0909 0.0413 0.188

## ________________________________
## Ungrouped data, standard error

## On linear predictor scale (default)
d |> glm_plotdata(cbind(pn, qn), iv, conf_level = NA)
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n     obs    pred  lower  upper
#> * <fct> <fct>   <int>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1 a     NA         66 -0.0606 -0.0606 -0.307  0.186
#> 2 b     NA         66  0       0      -0.246  0.246
#> 3 c     NA         66 -0.495  -0.495  -0.748 -0.241
#> 4 d     NA         66 -1.41   -1.41   -1.71  -1.10 
#> 5 e     NA         66 -2.30   -2.30   -2.73  -1.87 

## On response scale
d |> glm_plotdata(cbind(pn, qn), iv, conf_level = NA, type = "response")
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n    obs   pred  lower upper
#> * <fct> <fct>   <int>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 a     NA         66 0.485  0.485  0.424  0.546
#> 2 b     NA         66 0.5    0.5    0.439  0.561
#> 3 c     NA         66 0.379  0.379  0.321  0.440
#> 4 d     NA         66 0.197  0.197  0.153  0.251
#> 5 e     NA         66 0.0909 0.0909 0.0612 0.133

(d <- list(iv2 = list(ab = c("a", "b"), cd = c("c", "d"))) |>
    add_grps(d, iv, .key = _))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 4
#>   iv    iv2      pn    qn
#>   <fct> <fct> <int> <int>
#> 1 a     ab       32    34
#> 2 b     ab       33    33
#> 3 c     cd       25    41
#> 4 d     cd       13    53
#> 5 e     e         6    60

## _________________________________________________
## Grouped data, 95% Confidence interval (default)

## On linear predictor scale (default)
d |> glm_plotdata(cbind(pn, qn), iv2, .ungroup = iv)
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n     obs    pred  lower  upper
#> * <fct> <fct>   <int>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1 a     ab         66 -0.0606 -0.0303 -0.373  0.312
#> 2 b     ab         66  0      -0.0303 -0.373  0.312
#> 3 c     cd         66 -0.495  -0.906  -1.28  -0.528
#> 4 d     cd         66 -1.41   -0.906  -1.28  -0.528
#> 5 e     e          66 -2.30   -2.30   -3.14  -1.46 

## On response scale
d |> glm_plotdata(cbind(pn, qn), iv2, .ungroup = iv, type = "response")
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n    obs   pred  lower upper
#> * <fct> <fct>   <int>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 a     ab         66 0.485  0.492  0.408  0.577
#> 2 b     ab         66 0.5    0.492  0.408  0.577
#> 3 c     cd         66 0.379  0.288  0.217  0.371
#> 4 d     cd         66 0.197  0.288  0.217  0.371
#> 5 e     e          66 0.0909 0.0909 0.0413 0.188

## ______________________________
## Grouped data, standard error

## On linear predictor scale (default)
d |> glm_plotdata(cbind(pn, qn), iv2, .ungroup = iv, conf_level = NA)
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n     obs    pred  lower  upper
#> * <fct> <fct>   <int>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1 a     ab         66 -0.0606 -0.0303 -0.204  0.144
#> 2 b     ab         66  0      -0.0303 -0.204  0.144
#> 3 c     cd         66 -0.495  -0.906  -1.10  -0.713
#> 4 d     cd         66 -1.41   -0.906  -1.10  -0.713
#> 5 e     e          66 -2.30   -2.30   -2.73  -1.87 

## On response scale
d |> glm_plotdata(
        cbind(pn, qn), iv2,
        .ungroup = iv, conf_level = NA, type = "response"
     )
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n    obs   pred  lower upper
#> * <fct> <fct>   <int>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 a     ab         66 0.485  0.492  0.449  0.536
#> 2 b     ab         66 0.5    0.492  0.449  0.536
#> 3 c     cd         66 0.379  0.288  0.250  0.329
#> 4 d     cd         66 0.197  0.288  0.250  0.329
#> 5 e     e          66 0.0909 0.0909 0.0612 0.133

rm(d)
```

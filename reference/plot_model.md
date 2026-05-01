# Plot Model Predictions with Error Bars for Univariable GLM

S3 method to enable
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) in
package
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
to plot `"glm_plotdata"` objects ouptut by
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md).

## Usage

``` r
# S3 method for class 'glm_plotdata'
ggplot(
  data = NULL,
  mapping = aes(),
  as_percent = FALSE,
  rev_y = FALSE,
  ...,
  environment = parent.frame()
)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- mapping:

  Default list of aesthetic mappings to use for plot. If not specified,
  must be supplied in each layer added to the plot.

- as_percent:

  `logical`. If `TRUE`, the y-axis uses a percentage scale; default
  `FALSE`.

- rev_y:

  `logical`. If `TRUE`, the direction of the y-axis is reversed, which
  may be useful when plotting linear predictors; default `FALSE`.

- ...:

  further arguments passed to or from other methods.

- environment:

  **\[deprecated\]** Used prior to tidy evaluation.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

This S3 method plots model predictions and error bars representing
confidence intervals or standard errors for a univariable glm with a
categorical independent variable, optionally allowing representation of
groupings of levels of the independent variable and faceting of a number
of such plots.

`ggplot.glm_plotdata()` recognises a `factor` or `character` column in
`data` named `grouped` for plotting grouped levels of an independent
variable that are grouped within the underlying model. If levels are
indeed grouped in the model, the data bars will be plotted with
colour-coded borders representing the groups, and the ungrouped observed
values contained in the `data` column `level` are plotted as symbols. If
ungrouped levels are to be plotted, the `grouped` column should only
contain [`NA`](https://rdrr.io/r/base/NA.html) values

A `character` column in `data` containing names of independent variables
to be used for faceting may be identified by setting an attribute
`"facet_by"` in `data`. Names of variables to be used for faceting may
be converted to more informative facet labels by using a user-defined,
vectorised
[`labeller()`](https://ggplot2.tidyverse.org/reference/labeller.html)
function which should be named
[`var_labs()`](https://mark-eis.github.io/ParaAnita/reference/var_labs.md),
see `labeller` under
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

If an individual plot, rather than a faceted plot, is printed, the name
of the independent variable, converted by
[`var_labs()`](https://mark-eis.github.io/ParaAnita/reference/var_labs.md)
(if provided), will be used for the plot title. The plot title, subtitle
and axis labels may be overridden using the usual
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
syntax, see examples.

## See also

[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html),
[`labeller()`](https://ggplot2.tidyverse.org/reference/labeller.html).

Other plot_model:
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md),
[`glm_plotlist()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotlist.md),
[`var_labs()`](https://mark-eis.github.io/ParaAnita/reference/var_labs.md)

## Examples

``` r
## Example uses randomly generated data; re-running may be worthwhile.  

oldtheme <- theme_get()   ## Save ggplot defaults for later restoration

## Set ggplot defaults for pretty printing
theme_update(
  plot.title = element_text(color = "black", size = 20, hjust = 0.5),
  plot.subtitle = element_text(color = "black", size = 18, hjust = 0.5),
  axis.text.x = element_text(color = "black", size = 15),
  axis.text.y = element_text(color = "black", size = 15),
  axis.title.x = element_text(color = "black", size = 15),
  axis.title.y = element_text(color = "black", size = 15),
  strip.text.x = element_text(color = "black", size = 15),
  legend.position = "none"
)

## "labeller()" function to provide plot titles - see var_labs()
var_labs <- as_labeller(
    c(iv = "Risk Factor (Ungrouped Levels)",
      iv2 = "Risk Factor (Grouped Levels)")
)

## Create binomial data with groupings
(d <- list(iv2 = list(ab = c("a", "b"), cd = c("c", "d"))) |>
    add_grps(binom_data(), iv, .key = _))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 4
#>   iv    iv2      pn    qn
#>   <fct> <fct> <int> <int>
#> 1 a     ab       32    34
#> 2 b     ab       29    37
#> 3 c     cd       22    44
#> 4 d     cd       11    55
#> 5 e     e         4    62

## Ungrouped GLM plot data on linear predictor scale
(dp <- glm_plotdata(d, cbind(pn, qn), iv))
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n     obs    pred  lower  upper
#> * <fct> <fct>   <int>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1 a     NA         66 -0.0606 -0.0606 -0.545  0.424
#> 2 b     NA         66 -0.244  -0.244  -0.732  0.244
#> 3 c     NA         66 -0.693  -0.693  -1.21  -0.179
#> 4 d     NA         66 -1.61   -1.61   -2.26  -0.960
#> 5 e     NA         66 -2.74   -2.74   -3.76  -1.73 

## Plot model predictions and CI error bars
dp |> ggplot()


## Plot model predictions and CI error bars with reversed y-axis
dp |> ggplot(rev_y = TRUE)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • rev_y = rev_y
#> ℹ Did you misspell an argument name?


## Grouped GLM plot data on linear predictor scale
(dp <- glm_plotdata(d, cbind(pn, qn), iv2, .ungroup = iv))
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n     obs   pred  lower  upper
#> * <fct> <fct>   <int>   <dbl>  <dbl>  <dbl>  <dbl>
#> 1 a     ab         66 -0.0606 -0.152 -0.495  0.192
#> 2 b     ab         66 -0.244  -0.152 -0.495  0.192
#> 3 c     cd         66 -0.693  -1.10  -1.49  -0.703
#> 4 d     cd         66 -1.61   -1.10  -1.49  -0.703
#> 5 e     e          66 -2.74   -2.74  -3.76  -1.73 

## Plot model predictions and CI error bars with reversed y-axis
dp |> ggplot(rev_y = TRUE)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • rev_y = rev_y
#> ℹ Did you misspell an argument name?


## Ungrouped GLM plot data on reponse scale
(dp <- glm_plotdata(d, cbind(pn, qn), iv, type = "response"))
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n    obs   pred  lower upper
#> * <fct> <fct>   <int>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 a     NA         66 0.485  0.485  0.367  0.604
#> 2 b     NA         66 0.439  0.439  0.325  0.561
#> 3 c     NA         66 0.333  0.333  0.230  0.455
#> 4 d     NA         66 0.167  0.167  0.0946 0.277
#> 5 e     NA         66 0.0606 0.0606 0.0229 0.151

## Plot model predictions and CI error bars
dp |> ggplot()


## Plot model predictions and CI error bars, with y-axis as percentage
dp |> ggplot(as_percent = TRUE)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • as_percent = as_percent
#> ℹ Did you misspell an argument name?


## Grouped GLM plot data on reponse scale
(dp <- glm_plotdata(d, cbind(pn, qn), iv2, .ungroup = iv, type = "response"))
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n    obs   pred  lower upper
#> * <fct> <fct>   <int>  <dbl>  <dbl>  <dbl> <dbl>
#> 1 a     ab         66 0.485  0.462  0.379  0.548
#> 2 b     ab         66 0.439  0.462  0.379  0.548
#> 3 c     cd         66 0.333  0.25   0.183  0.331
#> 4 d     cd         66 0.167  0.25   0.183  0.331
#> 5 e     e          66 0.0606 0.0606 0.0229 0.151

## Plot model predictions and CI error bars
dp |> ggplot(as_percent = TRUE)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • as_percent = as_percent
#> ℹ Did you misspell an argument name?


## Grouped GLM plot data on reponse scale with standard errors 
(dp <- glm_plotdata(
                    d, cbind(pn, qn), iv2,
                    .ungroup = iv, conf_level = NA, type = "response"
                   ))
#> ________________
#> GLM Plot Data: -
#> 
#> # A tibble: 5 × 7
#>   level grouped     n    obs   pred  lower  upper
#> * <fct> <fct>   <int>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 a     ab         66 0.485  0.462  0.419  0.506 
#> 2 b     ab         66 0.439  0.462  0.419  0.506 
#> 3 c     cd         66 0.333  0.25   0.214  0.290 
#> 4 d     cd         66 0.167  0.25   0.214  0.290 
#> 5 e     e          66 0.0606 0.0606 0.0371 0.0975

## Plot model predictions and standard error bars
dp |> ggplot(as_percent = TRUE)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • as_percent = as_percent
#> ℹ Did you misspell an argument name?


## Add x-axis label and bespoke titles
dp |> ggplot(as_percent = TRUE) +
labs(
    x = "Level",
    title = "Example for ggplot.glm_plotdata()",
    subtitle = "Fascinating Results"
)
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • as_percent = as_percent
#> ℹ Did you misspell an argument name?


theme_set(oldtheme)    ## Restore original ggplot defaults
rm(d, dp, oldtheme)
```

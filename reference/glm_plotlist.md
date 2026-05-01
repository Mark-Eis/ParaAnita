# Data for Plotting Univariable GLM Predictions and Error Bars for Multiple Independent Variables

`glm_plotlist()` formats data for plotting univariable GLM predictions
with error bars for each of a number of independent variables.

## Usage

``` r
glm_plotlist(
  data,
  .dep_var,
  ...,
  .ungroups = NULL,
  .conf_level = 0.95,
  .type = c("link", "response"),
  .facet_by = NULL
)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the response variable in the data, either as a
  `numeric vector` with values of `1` and `0`, representing success or
  failure respectively, or as a two-column `matrix` with the columns
  giving the numbers of successes and failures see
  [`glm()`](https://rdrr.io/r/stats/glm.html).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  independent variables to be included in the plot data.

- .ungroups:

  a named character vector of ungrouped levels of independent variables
  specified in `.ind_var`, see details; default `NULL`.

- .conf_level:

  the confidence level required for the error bars; default `0.95`. If
  `NA`, error bars are standard error.

- .type:

  the type of prediction required. The default is on the scale of the
  linear predictors; the alternative `"response"` is on the scale of the
  response variable; default `"link"`.

- .facet_by:

  `NULL`, the default; or, if the output is to be combined into a single
  object to be used for a faceted plot, a `character vector` of length
  one used to name an additional column containing the names of the
  independent variables.

## Value

If the argument `.facet_by` is `NULL`, a `list` of `"glm_plotdata"`
objects suitable for producing multiple plots using
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
Otherwise, a single `"glm_plotdata"` object with an additional column
taking its name from `.facet_by` and containing the names of the
independent variables.

## Details

`glm_plotlist()` invokes
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md)
to create a `list` of `"glm_plotdata"` objects for plotting univariable
GLM predictions with error bars for each of a number of independent
variables in `data`. Independent variables to be included are selected
using the `...` argument with the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax of package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of “selection helpers”.

Like
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md),
`glm_plotlist()` allows exploration of proposed groupings of levels of
independent variables (e.g. as obtained using
[`add_grps()`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md)
or
[`fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html))
and inclusion of both grouped and ungrouped levels in the
`"glm_plotdata"` objects comprising its output list. In such cases, the
`.ungroups` argument is used to provide a named `character vector` of
the names of the corresponding factors in `data` giving the grouped and
ungrouped levels of the form `ungrouped_name = "grouped_name"`; levels
not otherwise mentioned will be left as is.

The grouped levels are used as the independent variable in the GLM
invoked by
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md)
and are output in the column `grouped` within the corresponding
`"glm_plotdata"` object, while the ungrouped levels are shown in the
column `level`, see
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md).

The `.conf_level` and `.type` arguments are handled as for
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md).

`glm_plotlist()` may be used in conjunction with
[`lapply()`](https://rdrr.io/r/base/lapply.html) (or
[purrr](https://purrr.tidyverse.org/reference/purrr-package.html)
package [map()](https://purrr.tidyverse.org/reference/map.html)) to
rapidly obtain multiple plots of univariable GLMs for a number of
independent variables.

Levels of independent variables for which the observed values are all
zero or all one are not included in the output, although they are taken
into consideration in calculating denominators in the case of grouped
levels.

## See also

[`add_grps()`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md),
[`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
and
[`fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html).

Other plot_model:
[`Plot_Model`](https://mark-eis.github.io/ParaAnita/reference/plot_model.md),
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md),
[`var_labs()`](https://mark-eis.github.io/ParaAnita/reference/var_labs.md)

## Examples

``` r
# Coming soon!
```

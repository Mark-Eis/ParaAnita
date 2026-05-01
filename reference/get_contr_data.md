# Get and Set Treatment Contrasts for Independent Variables in Data

`get_contr_data()` shows the `"contrasts"` attributes of all or selected
factors within a data frame.

`set_contr_treat()` sets the `"contrasts"` attribute for selected
factors within a data frame to a treatment contrast matrix with
individually specified baseline levels.

`set_contr_treat()<-` is the replacement function form.

## Usage

``` r
get_contr_data(data, ...)

set_contr_treat(data, ..., .baseline = NULL, .verbose = TRUE)

set_contr_treat(data, ..., .verbose = FALSE) <- value
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The selection of one or more factors in `data` for getting or setting
  contrasts.

- .baseline:

  a `numeric` vector of `length` equal to the number of contrasts to be
  set specifying which level is considered the baseline.

- .verbose:

  `logical`, whether to print "before and after" contrast matrices for
  factors in `data`.

- value:

  `numeric`, see `.baseline` argument.

## Value

The original dataframe or
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
with the `"contrasts"` attributes set for selected `factors`.

## Details

`get_contr_data()` returns the `"contrasts"` attributes of all or
selected factors.

Factors in `.data` may be selected for getting and setting contrasts
using the `...` argument with the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax of package dplyr, including use of **selection helpers**. If no
`...` arguments are supplied, all categorical variables in `data` (i.e.,
`character` or `factor` columns) will be selected.

The `"contrasts"` attribute of `factors` selected with `...` are set
using the contrast function `contr.treatment` with baseline factor
levels as specified numerically in the argument `.baseline` or the
`value` argument in the case of the replacement function form
`set_contrasts()<-`. If no `.baseline` argument is supplied, by default
the first factor level is used as baseline.

The individual `.baseline` (or `.value`) argument values are capped to
be no greater than [`nlevels`](https://rdrr.io/r/base/nlevels.html) for
each of the corresponding factors selected in `...`. Hence, to ensure
the last level is the reference level, a baseline value can be specified
as a large integer (e.g., `99`L), which may be convenient when using
`set_contr_treat()` programmatically.

## See also

[`contrasts`](https://rdrr.io/r/stats/contrasts.html), `C`,
`contr.treatment`.

Other get-contrasts:
[`get_contrasts()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md)

## Examples

``` r
# Coming soon!
```

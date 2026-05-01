# Format or Lookup Variable Names for Plot Titles

Vectorised labeller function used by
[`ggplot.glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/plot_model.md)
for revising variable names for use as subtitles in individual plots or
as facet labels in faceted plots.

## Usage

``` r
var_labs(labels)
```

## Arguments

- labels:

  character vector containing the names of the variables to be revised.

## Value

A character vector containing the revised names.

## Details

`var_labs` in package
[ParaAnita](https://mark-eis.github.io/ParaAnita/reference/ParaAnita-package.md)
simply applies
[`str_to_title`](https://stringr.tidyverse.org/reference/case.html) to
its argument. The user may override this by providing their own
vectorised
[`labeller`](https://ggplot2.tidyverse.org/reference/labeller.html)
function, see `labeller` under
[`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
and the example.

## See also

[`as_labeller`](https://ggplot2.tidyverse.org/reference/as_labeller.html),
[`facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
[`labeller`](https://ggplot2.tidyverse.org/reference/labeller.html).

Other plot_model:
[`Plot_Model`](https://mark-eis.github.io/ParaAnita/reference/plot_model.md),
[`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md),
[`glm_plotlist()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotlist.md)

## Examples

``` r

## Default labeller
c("matthew", "mark", "luke", "john") |> var_labs()
#> [[1]]
#> [1] "Matthew"
#> 
#> [[2]]
#> [1] "Mark"
#> 
#> [[3]]
#> [1] "Luke"
#> 
#> [[4]]
#> [1] "John"
#> 

ygps <- c("year", "ygroup1", "ygroup2", "ygroup3", "ygroup4", "ygroup5", "ygroup6", "ygroup7")
mgps <- c("month", "season", "mgroup2", "mgroup3", "mgroup4", "mgroup5", "mgroup6")
demog <- c("gender", "age_group", "location", "breed")

# Vectorised function to replace terse variable names with names suitable for labelling plots
var_labs <- as_labeller(
    c(
        c("Year", paste("Year Group", seq_along(ygps[-1]))) |> set_names(ygps),
        c("Month", "Season", paste("Month Group", seq_along(mgps[-1])[-1])) |> set_names(mgps),
        c("Animal Gender", "Age Group", "Geographic Location", "Cattle Breed") |> set_names(demog)
    )
)

ygps |> var_labs()
#> [[1]]
#> [1] "Year"
#> 
#> [[2]]
#> [1] "Year Group 1"
#> 
#> [[3]]
#> [1] "Year Group 2"
#> 
#> [[4]]
#> [1] "Year Group 3"
#> 
#> [[5]]
#> [1] "Year Group 4"
#> 
#> [[6]]
#> [1] "Year Group 5"
#> 
#> [[7]]
#> [1] "Year Group 6"
#> 
#> [[8]]
#> [1] "Year Group 7"
#> 
mgps |> var_labs()
#> [[1]]
#> [1] "Month"
#> 
#> [[2]]
#> [1] "Season"
#> 
#> [[3]]
#> [1] "Month Group 2"
#> 
#> [[4]]
#> [1] "Month Group 3"
#> 
#> [[5]]
#> [1] "Month Group 4"
#> 
#> [[6]]
#> [1] "Month Group 5"
#> 
#> [[7]]
#> [1] "Month Group 6"
#> 
demog |> var_labs()
#> [[1]]
#> [1] "Animal Gender"
#> 
#> [[2]]
#> [1] "Age Group"
#> 
#> [[3]]
#> [1] "Geographic Location"
#> 
#> [[4]]
#> [1] "Cattle Breed"
#> 

rm(demog, mgps, var_labs, ygps)
```

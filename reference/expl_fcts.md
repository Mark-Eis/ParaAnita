# Explanatory Factors in Data as List of Expressions

Create a list of defused expressions representing the names of all or a
selection of explanatory factors or character vectors in a dataset.

## Usage

``` r
expl_fcts(
  .data,
  ...,
  .named = FALSE,
  .val = c("syms", "data_syms", "character")
)
```

## Arguments

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  quoted name(s) of one or more `factors` or `character vectors` in
  `.data`, to be included in (or excluded from) the output.

- .named:

  `logical`, whether to name the elements of the list. If `TRUE`,
  unnamed inputs are automatically named with
  [`set_names()`](https://rlang.r-lib.org/reference/set_names.html);
  default `FALSE`.

- .val:

  the type of output required. The default `"syms"` returns a list of
  `symbols`; the alternative `"data_syms"` returns a list of `symbols`
  prefixed with the
  [`.data`](https://rlang.r-lib.org/reference/dot-data.html) pronoun.
  The `"character"` option returns a `character vector`.

## Value

A `list` of `symbols` representing the names of selected explanatory
`factors` or `character vectors` in `.data`; unless
`.val = "data_syms"`, in which case the `symbols` are prefixed with the
[`.data`](https://rlang.r-lib.org/reference/dot-data.html) pronoun or
`.val = "character"` whereupon the selected names are returned as a
`character vector` instead.

## Details

By default, `expl_fcts()` creates a
[`list`](https://rdrr.io/r/base/list.html) of
[`symbols`](https://rdrr.io/r/base/name.html) i.e., [defused R
expressions](https://rlang.r-lib.org/reference/topic-defuse.html),
representing the names of all or a selection of explanatory factors (or
character vectors) in `.data`, using
[`syms()`](https://rlang.r-lib.org/reference/sym.html) from package
[rlang](https://rlang.r-lib.org/reference/rlang-package.html).
Alternatively, if `.val = "data_syms"`, a list of symbols prefixed with
the [`.data`](https://rlang.r-lib.org/reference/dot-data.html) pronoun
is returned instead. Finally, if `.val = "character"`, `expl_fcts()`
returns a `character vector` of the names of the explanatory factors (or
character vectors) in `.data`

Variables in `.data` may be selected for inclusion or exclusion using
the `...` argument and the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax from package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of “selection helpers”. If no `...` arguments are
supplied, all categorical variables in `.data` will be included in the
list.

A list of `symbols` returned by `expl_fcts()` may be “injected” into the
`...` arguments of
[`contingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md),
[`xcontingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md),
[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
and other similar functions, using the
[splice-operator](https://rlang.r-lib.org/reference/splice-operator.html)
`!!!`. If `.val = "character"`, the functions
[`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html) or
[`any_of()`](https://tidyselect.r-lib.org/reference/all_of.html) should
be used to wrap the resulting `character vector` of names instead of
using `!!!`. A list of `symbols` returned by `expl_fcts()` may also be
used to provide a list argument with injection support to
[`lapply()`](https://rdrr.io/r/base/lapply.html) (or
[purrr](https://purrr.tidyverse.org/reference/purrr-package.html)
package [map()](https://purrr.tidyverse.org/reference/map.html)
functions), using the
[injection-operator](https://rlang.r-lib.org/reference/injection-operator.html)
`!!` (see examples).

## See also

[`!!`](https://rlang.r-lib.org/reference/injection-operator.html),
[`!!!`](https://rlang.r-lib.org/reference/splice-operator.html),
[`all_of`](https://tidyselect.r-lib.org/reference/all_of.html),
[`any_of`](https://tidyselect.r-lib.org/reference/all_of.html),
[`set_names()`](https://rlang.r-lib.org/reference/set_names.html),
[`defused R expressions`](https://rlang.r-lib.org/reference/topic-defuse.html),
[`map()`](https://purrr.tidyverse.org/reference/map.html) and
[`symbol`](https://rdrr.io/r/base/name.html).

Other contingency_table:
[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md),
[`contingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md)

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
#>  2 a     g     i     k         0
#>  3 a     g     i     k         0
#>  4 a     g     i     k         0
#>  5 a     g     i     k         1
#>  6 a     g     i     k         0
#>  7 a     g     i     k         0
#>  8 a     g     i     k         0
#>  9 a     g     i     k         1
#> 10 a     g     i     k         0
#> # ℹ 386 more rows

d |> expl_fcts()
#> [[1]]
#> iv
#> 
#> [[2]]
#> iv2
#> 
#> [[3]]
#> iv3
#> 
#> [[4]]
#> iv4
#> 

d |> expl_fcts(.named = TRUE)
#> $iv
#> iv
#> 
#> $iv2
#> iv2
#> 
#> $iv3
#> iv3
#> 
#> $iv4
#> iv4
#> 

d |> expl_fcts(.val = "data_syms")
#> [[1]]
#> .data$iv
#> 
#> [[2]]
#> .data$iv2
#> 
#> [[3]]
#> .data$iv3
#> 
#> [[4]]
#> .data$iv4
#> 

d |> expl_fcts(.named = TRUE, .val = "data_syms")
#> $iv
#> .data$iv
#> 
#> $iv2
#> .data$iv2
#> 
#> $iv3
#> .data$iv3
#> 
#> $iv4
#> .data$iv4
#> 

d |> expl_fcts(.val = "character")
#> [1] "iv"  "iv2" "iv3" "iv4"

d |> expl_fcts(.named = TRUE, .val = "character")
#>    iv   iv2   iv3   iv4 
#>  "iv" "iv2" "iv3" "iv4" 

## Select or exclude factors
d |> expl_fcts(iv, iv3)
#> [[1]]
#> iv
#> 
#> [[2]]
#> iv3
#> 

d |> expl_fcts(!c(iv, iv3))
#> [[1]]
#> iv2
#> 
#> [[2]]
#> iv4
#> 

## Use {dplyr} selection helpers e.g., last_col(), num_range() and starts_with()
d |> expl_fcts(last_col(1L))  ## Offset of 1L used, since last column of d is dv
#> [[1]]
#> iv4
#> 

d |> expl_fcts(!last_col())
#> [[1]]
#> iv
#> 
#> [[2]]
#> iv2
#> 
#> [[3]]
#> iv3
#> 
#> [[4]]
#> iv4
#> 

d |> expl_fcts(num_range("iv", 2:3))
#> [[1]]
#> iv2
#> 
#> [[2]]
#> iv3
#> 

d |> expl_fcts(!num_range("iv", 2:3))
#> [[1]]
#> iv
#> 
#> [[2]]
#> iv4
#> 

d |> expl_fcts(starts_with("iv"))
#> [[1]]
#> iv
#> 
#> [[2]]
#> iv2
#> 
#> [[3]]
#> iv3
#> 
#> [[4]]
#> iv4
#> 

## Negation of selection helper excludes all explanatory factors
d |> expl_fcts(!starts_with("iv"))
#> list()

## In following three examples, each triplet should give identical results
## Include all explanatory factors
d |> binom_contingency(dv)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        28    38
#> 2 b     h     i     k        27    39
#> 3 c     g     i     l        19    47
#> 4 d     h     j     l        23    43
#> 5 e     g     j     m         7    59
#> 6 f     h     j     m         3    63

d |> binom_contingency(dv, !!!expl_fcts(d))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        28    38
#> 2 b     h     i     k        27    39
#> 3 c     g     i     l        19    47
#> 4 d     h     j     l        23    43
#> 5 e     g     j     m         7    59
#> 6 f     h     j     m         3    63

d |> binom_contingency(dv, all_of(expl_fcts(d, .val = "character")))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#> * <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        28    38
#> 2 b     h     i     k        27    39
#> 3 c     g     i     l        19    47
#> 4 d     h     j     l        23    43
#> 5 e     g     j     m         7    59
#> 6 f     h     j     m         3    63

## Include only iv and iv3
d |> binom_contingency(dv, iv, iv3)
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 4
#>   iv    iv3      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 a     i        28    38
#> 2 b     i        27    39
#> 3 c     i        19    47
#> 4 d     j        23    43
#> 5 e     j         7    59
#> 6 f     j         3    63

d |> binom_contingency(dv, !!!expl_fcts(d, iv, iv3))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 4
#>   iv    iv3      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 a     i        28    38
#> 2 b     i        27    39
#> 3 c     i        19    47
#> 4 d     j        23    43
#> 5 e     j         7    59
#> 6 f     j         3    63

d |> binom_contingency(dv, all_of(expl_fcts(d, iv, iv3, .val = "character")))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 4
#>   iv    iv3      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 a     i        28    38
#> 2 b     i        27    39
#> 3 c     i        19    47
#> 4 d     j        23    43
#> 5 e     j         7    59
#> 6 f     j         3    63

## Exclude iv and iv3
d |> binom_contingency(dv, !c(iv, iv3))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 4
#>   iv2   iv4      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 g     k        28    38
#> 2 h     k        27    39
#> 3 g     l        19    47
#> 4 h     l        23    43
#> 5 g     m         7    59
#> 6 h     m         3    63

d |> binom_contingency(dv, !!!expl_fcts(d, !c(iv, iv3)))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 4
#>   iv2   iv4      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 g     k        28    38
#> 2 h     k        27    39
#> 3 g     l        19    47
#> 4 h     l        23    43
#> 5 g     m         7    59
#> 6 h     m         3    63

d |> binom_contingency(dv, all_of(expl_fcts(d, !c(iv, iv3), .val = "character")))
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 4
#>   iv2   iv4      pn    qn
#> * <fct> <fct> <int> <int>
#> 1 g     k        28    38
#> 2 h     k        27    39
#> 3 g     l        19    47
#> 4 h     l        23    43
#> 5 g     m         7    59
#> 6 h     m         3    63

## Use with lapply, binom_contingency(), glm() and odds_ratio()
expl_fcts(d, .named = TRUE) |>
    lapply(\(x) binom_contingency(d, dv, !!x))
#> $iv
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 6 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        28    38
#> 2 b        27    39
#> 3 c        19    47
#> 4 d        23    43
#> 5 e         7    59
#> 6 f         3    63
#> 
#> $iv2
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 2 × 3
#>   iv2      pn    qn
#> * <fct> <int> <int>
#> 1 g        54   144
#> 2 h        53   145
#> 
#> $iv3
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 2 × 3
#>   iv3      pn    qn
#> * <fct> <int> <int>
#> 1 i        74   124
#> 2 j        33   165
#> 
#> $iv4
#> _____________________________
#> Binomial Contingency Table: -
#> 
#> # A tibble: 3 × 3
#>   iv4      pn    qn
#> * <fct> <int> <int>
#> 1 k        55    77
#> 2 l        42    90
#> 3 m        10   122
#> 

expl_fcts(d, .named = TRUE) |>
    lapply(\(x)
        binom_contingency(d, dv, !!x) |>
        glm(cbind(pn, qn) ~ ., binomial, data = _)
    )
#> $iv
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ ., family = binomial, data = binom_contingency(d, 
#>     dv, !!x))
#> 
#> Coefficients:
#> (Intercept)          ivb          ivc          ivd          ive          ivf  
#>    -0.30538     -0.06234     -0.60033     -0.32032     -1.82625     -2.73914  
#> 
#> Degrees of Freedom: 5 Total (i.e. Null);  0 Residual
#> Null Deviance:       49.2 
#> Residual Deviance: 1.754e-14     AIC: 36.89
#> 
#> $iv2
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ ., family = binomial, data = binom_contingency(d, 
#>     dv, !!x))
#> 
#> Coefficients:
#> (Intercept)         iv2h  
#>    -0.98083     -0.02561  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       0.01281 
#> Residual Deviance: 4.441e-16     AIC: 15.01
#> 
#> $iv3
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ ., family = binomial, data = binom_contingency(d, 
#>     dv, !!x))
#> 
#> Coefficients:
#> (Intercept)         iv3j  
#>     -0.5162      -1.0932  
#> 
#> Degrees of Freedom: 1 Total (i.e. Null);  0 Residual
#> Null Deviance:       21.96 
#> Residual Deviance: 6.595e-14     AIC: 14.83
#> 
#> $iv4
#> 
#> Call:  glm(formula = cbind(pn, qn) ~ ., family = binomial, data = binom_contingency(d, 
#>     dv, !!x))
#> 
#> Coefficients:
#> (Intercept)         iv4l         iv4m  
#>     -0.3365      -0.4257      -2.1650  
#> 
#> Degrees of Freedom: 2 Total (i.e. Null);  0 Residual
#> Null Deviance:       46.84 
#> Residual Deviance: -6.706e-14    AIC: 20.59
#> 

expl_fcts(d, .named = TRUE) |>
    lapply(\(x)
        binom_contingency(d, dv, !!x, .drop_zero = TRUE) |>
        odds_ratio(.ind_var = !!x)
    )
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> Waiting for profiling to be done...
#> $iv
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 6 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)  -0.305  0.249 0.220         1          NA          NA     NS   
#> 2 ivb          -0.0623 0.353 0.860         0.940       0.469       1.88  NS   
#> 3 ivc          -0.600  0.369 0.103         0.549       0.263       1.12  NS   
#> 4 ivd          -0.320  0.359 0.372         0.726       0.357       1.46  NS   
#> 5 ive          -1.83   0.471 0.000106      0.161       0.0597      0.387 ***  
#> 6 ivf          -2.74   0.641 0.0000194     0.0646      0.0147      0.198 ***  
#> 
#> $iv2
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 2 × 7
#>   parameter   estimate    se p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl> <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)  -0.981  0.160 0          1          NA          NA    ***  
#> 2 iv2h         -0.0256 0.226 0.910      0.975       0.625       1.52 NS   
#> 
#> $iv3
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 2 × 7
#>   parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -0.516 0.147 0.000441       1          NA         NA     ***  
#> 2 iv3j          -1.09  0.241 0.0000056      0.335       0.207      0.533 ***  
#> 
#> $iv4
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 3 × 7
#>   parameter   estimate    se  p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>   <chr>          <dbl> <dbl>  <dbl>      <dbl>       <dbl>      <dbl> <fct>
#> 1 (Intercept)   -0.336 0.177 0.0567      1         NA          NA     .    
#> 2 iv4l          -0.426 0.257 0.0978      0.653      0.393       1.08  .    
#> 3 iv4m          -2.16  0.373 0           0.115      0.0524      0.229 ***  
#> 

rm(d)
```

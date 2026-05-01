# Add Factors to Data Based on Grouped Levels of an Existing Factor

Add new factors to data based on grouped levels of an existing factor,
using a key compatible with `fct_collapse`.

## Usage

``` r
add_grps(data, .fct, .key, .sort = TRUE)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .fct:

  the quoted name of an existing (ungrouped) `factor`.

- .key:

  a `list` of nested, named lists representing the groupings, each
  containing a series of named `character vectors`.

- .sort:

  `logical`, whether to sort levels of new factors; default `TRUE`.

## Value

A data frame, or a data frame extension (e.g. a
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)),
equivalent to `data` with the additional grouped factor(s).

## Details

The `.key` argument should be a series of named lists nested within an
outer list. Each nested named list must contain one or more named
`character vector`s representing the new factor groupings. The nested
lists should be structured for compatibility with
[`fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html)
in package forcats.

`add_grps()` will add new, grouped factors to `data`, one for each
nested list and with the same name. Levels are assigned to these new
grouped factors using the name of whichever character vector, if any,
contains the old factor level. If none does, the original ungrouped
factor level is used.

Various different groupings of a
[`factor`](https://rdrr.io/r/base/factor.html) may be conveniently added
to `data` using `add_grps()` and the corresponding series of related
binomial [`glm`](https://rdrr.io/r/stats/glm.html)s compared using
[`comp_glm()`](https://mark-eis.github.io/ParaAnita/reference/comp_glm.md).

## See also

[`comp_glm()`](https://mark-eis.github.io/ParaAnita/reference/comp_glm.md),
[`fct_collapse()`](https://forcats.tidyverse.org/reference/fct_collapse.html),
[`list()`](https://rdrr.io/r/base/list.html).

Other factor-manip:
[`fct_to_num()`](https://mark-eis.github.io/ParaAnita/reference/fct_to_num.md)

## Examples

``` r
(d <- binom_data(levels = 6))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 6 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        33    33
#> 2 b        21    45
#> 3 c        22    44
#> 4 d        15    51
#> 5 e        12    54
#> 6 f        12    54

## One grouped factor
(grp_key <- list(g = c("a", "c", "e"), h = c("b", "d", "f")))
#> $g
#> [1] "a" "c" "e"
#> 
#> $h
#> [1] "b" "d" "f"
#> 

d |> add_grps(iv, list(iv2 = grp_key))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 6 × 4
#>   iv    iv2      pn    qn
#>   <fct> <fct> <int> <int>
#> 1 a     g        33    33
#> 2 b     h        21    45
#> 3 c     g        22    44
#> 4 d     h        15    51
#> 5 e     g        12    54
#> 6 f     h        12    54

## Several grouped factors
grp_key <- list(
    iv2 = grp_key,
    iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
    iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
)

d |> add_grps(iv, grp_key)
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#>   <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        33    33
#> 2 b     h     i     k        21    45
#> 3 c     g     i     l        22    44
#> 4 d     h     j     l        15    51
#> 5 e     g     j     m        12    54
#> 6 f     h     j     m        12    54

## Cut out the middleman
list(
    iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
    iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
    iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
) |>
add_grps(d, iv, .key = _)
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 6 × 6
#>   iv    iv2   iv3   iv4      pn    qn
#>   <fct> <fct> <fct> <fct> <int> <int>
#> 1 a     g     i     k        33    33
#> 2 b     h     i     k        21    45
#> 3 c     g     i     l        22    44
#> 4 d     h     j     l        15    51
#> 5 e     g     j     m        12    54
#> 6 f     h     j     m        12    54

## Binomial data with month as explanatory variable, using dplyr and forcats package functions
(d <- binom_data(12, probs = rep_len(0.5, 12)) |>
    mutate(across(iv, \(x) fct_recode(x, !!!setNames(letters[1:12], month.abb)))) |>
    rename(month = "iv"))
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 12 × 3
#>    month    pn    qn
#>    <fct> <int> <int>
#>  1 Jan      31    35
#>  2 Feb      30    36
#>  3 Mar      31    35
#>  4 Apr      28    38
#>  5 May      35    31
#>  6 Jun      33    33
#>  7 Jul      35    31
#>  8 Aug      36    30
#>  9 Sep      38    28
#> 10 Oct      29    37
#> 11 Nov      25    41
#> 12 Dec      35    31

## Name three lists of different month groupings using lapply()
(grp_key <- list(
    list(1:3, 4:6, 7:9, 10:12),
    list(1:4, 5:8, 9:12),
    list(c(1:3, 10:12), 4:9)
) |>
lapply(\(x) lapply(x, \(y) month.abb[y])) |>
lapply(\(x) setNames(x, paste0("group", seq_along(x)))) |>
(\(x) setNames(x, paste0("months", seq_along(x))))())
#> $months1
#> $months1$group1
#> [1] "Jan" "Feb" "Mar"
#> 
#> $months1$group2
#> [1] "Apr" "May" "Jun"
#> 
#> $months1$group3
#> [1] "Jul" "Aug" "Sep"
#> 
#> $months1$group4
#> [1] "Oct" "Nov" "Dec"
#> 
#> 
#> $months2
#> $months2$group1
#> [1] "Jan" "Feb" "Mar" "Apr"
#> 
#> $months2$group2
#> [1] "May" "Jun" "Jul" "Aug"
#> 
#> $months2$group3
#> [1] "Sep" "Oct" "Nov" "Dec"
#> 
#> 
#> $months3
#> $months3$group1
#> [1] "Jan" "Feb" "Mar" "Oct" "Nov" "Dec"
#> 
#> $months3$group2
#> [1] "Apr" "May" "Jun" "Jul" "Aug" "Sep"
#> 
#> 

add_grps(d, month, grp_key)        ## Add the new year groups to data
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 12 × 6
#>    month months1 months2 months3    pn    qn
#>    <fct> <fct>   <fct>   <fct>   <int> <int>
#>  1 Jan   group1  group1  group1     31    35
#>  2 Feb   group1  group1  group1     30    36
#>  3 Mar   group1  group1  group1     31    35
#>  4 Apr   group2  group1  group2     28    38
#>  5 May   group2  group2  group2     35    31
#>  6 Jun   group2  group2  group2     33    33
#>  7 Jul   group3  group2  group2     35    31
#>  8 Aug   group3  group2  group2     36    30
#>  9 Sep   group3  group3  group2     38    28
#> 10 Oct   group4  group3  group1     29    37
#> 11 Nov   group4  group3  group1     25    41
#> 12 Dec   group4  group3  group1     35    31

## Example from fct_collapse() using gss_cat dataset from {forcats} package

fct_count(gss_cat$partyid)
#> # A tibble: 10 × 2
#>    f                      n
#>    <fct>              <int>
#>  1 No answer            154
#>  2 Don't know             1
#>  3 Other party          393
#>  4 Strong republican   2314
#>  5 Not str republican  3032
#>  6 Ind,near rep        1791
#>  7 Independent         4119
#>  8 Ind,near dem        2499
#>  9 Not str democrat    3690
#> 10 Strong democrat     3490

grp_key <- list(
    partyid2 = list(
        missing = c("No answer", "Don't know"),
        other = "Other party",
        rep = c("Strong republican", "Not str republican"),
        ind = c("Ind,near rep", "Independent", "Ind,near dem"),
        dem = c("Not str democrat", "Strong democrat")
    )
)

gss_cat |>
    add_grps(partyid, grp_key) |>
    _$partyid2 |> fct_count()
#> # A tibble: 5 × 2
#>   f           n
#>   <fct>   <int>
#> 1 dem      7180
#> 2 ind      8409
#> 3 missing   155
#> 4 other     393
#> 5 rep      5346

gss_cat |>
    add_grps(partyid, grp_key, .sort = FALSE) |>
    _$partyid2 |> fct_count()
#> # A tibble: 5 × 2
#>   f           n
#>   <fct>   <int>
#> 1 missing   155
#> 2 other     393
#> 3 rep      5346
#> 4 ind      8409
#> 5 dem      7180


rm(grp_key, d)
```

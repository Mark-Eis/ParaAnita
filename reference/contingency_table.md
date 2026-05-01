# Contingency Tables for Two or More Categorical Variables

`contingency_table()` compiles a contingency table for two or more
categorical variables, the first of which is typically an outcome
(dependent) varable to be used for the column headings, while the
remainder are typically explanatory (independent) variables that will
appear in the contingency table either as factors or optionally as row
headings.

`xcontingency_table()` compiles a contingency table for a categorical
outcome varable and multiple categorical explanatory variables that are
“crossed” to obtain a single explanatory factor.

## Usage

``` r
contingency_table(.data, .dep_var, ..., .wt = NULL, .rownames = FALSE)

xcontingency_table(
  .data,
  .dep_var,
  ...,
  .crossname = NULL,
  .wt = NULL,
  .rownames = FALSE
)
```

## Arguments

- .data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .dep_var:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of the dependent variable, which may be a
  `character vector`, `factor`, or `numeric`.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  quoted name(s) of one or more `factors` or `character vectors` in
  `.data`, to be included in (or excluded from) the output.

- .wt:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of a numeric column in `.data` containing frequency
  weights; default `NULL`.

- .rownames:

  `logical`. If `TRUE`, value is a data frame with the levels of the
  first (or crossed) independent variable as row names, rather than a
  tibble; default `FALSE`.

- .crossname:

  a character string to be used as the name of the column of for the
  crossed variables. If omitted, the names of the crossed variables are
  used combined in “snake case”.

## Value

For `contingency_table()`, an object of class `"contingency_table"`,
`"announce"`, inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html),
or a [`data.frame`](https://rdrr.io/r/base/data.frame.html), depending
on whether `.rownames = FALSE` (default) or `TRUE`.

Similarly for `xcontingency_table()`, an object of class
`"xcontingency_table"`, `"announce"` inheriting from
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) or a
`data.frame`, again depending on the value of `rownames`.

## Details

Categorical variables (i.e. factors or character vectors) in `.data`
required as factors in the resulting contingency table may be selected
for inclusion or exclusion using the `...` argument and the
\<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
syntax from package
[dplyr](https://dplyr.tidyverse.org/reference/dplyr-package.html),
including use of “selection helpers”. If no `...` arguments are
supplied, all categorical variables in `.data` (other than `.dep_var`)
will be used.

A [list](https://rdrr.io/r/base/list.html) of [defused R
expressions](https://rlang.r-lib.org/reference/topic-defuse.html), as
for instance created by
[`expl_fcts()`](https://mark-eis.github.io/ParaAnita/reference/expl_fcts.md),
may be used as the `...` arguments and should be injected using the
[splice-operator](https://rlang.r-lib.org/reference/splice-operator.html),
`!!!`, see examples.

If `.wt = NULL`, the number of rows for each unique combination of the
dependent and independent variables are counted. If `.wt` is the quoted
name of a numeric variable representing frequency weights, these are
summated for each unique combination of the dependent and independent
variables.

If `.rownames = TRUE`, the resulting contingency table will be a
conventional `data.frame` rather than a `tibble` and the first
categorical variable (other than `.dep_var`) will be used for row
headings rather than as a factor. Having row headings allows the result
to be passed as an argument to
[`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html),
[`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) or
[`chsqfish()`](https://mark-eis.github.io/ParaAnita/reference/chsqfish.md),
e.g., conveniently using `|>` in a [piped
sequence](https://rdrr.io/r/base/pipeOp.html) (see examples). However,
using `.rownames = TRUE` for a contingency table with more than one
explanatory (independent) variable will most likely result in the error
message “duplicate 'row.names' are not allowed”, in which case
`xcontingency_table()` should be used instead.

Multiple categorical explanatory variables in a contingency table
compiled by `xcontingency_table()` are “crossed” using
[`fct_cross()`](https://forcats.tidyverse.org/reference/fct_cross.html).

## See also

[`defused R expressions`](https://rlang.r-lib.org/reference/topic-defuse.html),
[`fct_cross()`](https://forcats.tidyverse.org/reference/fct_cross.html),
[`splice-operator`](https://rlang.r-lib.org/reference/splice-operator.html)
and
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html);
[`Print_Methods`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
for S3 method for printing objects of class `"contingency_table"`.

Other contingency_table:
[`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md),
[`expl_fcts()`](https://mark-eis.github.io/ParaAnita/reference/expl_fcts.md)

## Examples

``` r
(d <- tibble(
    iv = letters[1:4] |> sample(10, replace = TRUE),
    dv = c(0L:3L) |> sample(10, replace = TRUE)
))
#> # A tibble: 10 × 2
#>    iv       dv
#>    <chr> <int>
#>  1 b         2
#>  2 a         1
#>  3 b         0
#>  4 b         2
#>  5 d         2
#>  6 b         0
#>  7 d         2
#>  8 c         3
#>  9 a         3
#> 10 d         1

d |> contingency_table(dv)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 4 × 5
#>   iv      `2`   `1`   `0`   `3`
#> * <chr> <int> <int> <int> <int>
#> 1 b         2     0     2     0
#> 2 a         0     1     0     1
#> 3 d         2     1     0     0
#> 4 c         0     0     0     1

d |> contingency_table(dv, .rownames = TRUE)
#>   2 1 0 3
#> b 2 0 2 0
#> a 0 1 0 1
#> d 2 1 0 0
#> c 0 0 0 1

## Use .data pronoun for more informative error messages
d |> contingency_table(.data$dv)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 4 × 5
#>   iv      `2`   `1`   `0`   `3`
#> * <chr> <int> <int> <int> <int>
#> 1 b         2     0     2     0
#> 2 a         0     1     0     1
#> 3 d         2     1     0     0
#> 4 c         0     0     0     1

try(d |> contingency_table(dx))
#> Error : object 'dx' not found

try(d |> contingency_table(.data$dx))
#> Error in .data$dx : Column `dx` not found in `.data`.

(d <- tibble(
    iv = letters[1:4] |> sample(10, replace = TRUE) |> as.factor(),
    dv = c("Success", "Fail", "Borderline")  |> sample(10, replace = TRUE)
  ))
#> # A tibble: 10 × 2
#>    iv    dv        
#>    <fct> <chr>     
#>  1 d     Borderline
#>  2 b     Borderline
#>  3 d     Borderline
#>  4 d     Fail      
#>  5 d     Fail      
#>  6 d     Borderline
#>  7 b     Fail      
#>  8 a     Success   
#>  9 c     Success   
#> 10 c     Borderline

d |> contingency_table(dv)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 4 × 4
#>   iv    Borderline  Fail Success
#> * <fct>      <int> <int>   <int>
#> 1 d              3     2       0
#> 2 b              1     1       0
#> 3 a              0     0       1
#> 4 c              1     0       1

d |> contingency_table(dv, .rownames = TRUE)
#>   Borderline Fail Success
#> d          3    2       0
#> b          1    1       0
#> a          0    0       1
#> c          1    0       1

(d <- tibble(
    iv = letters[1:4] |> sample(100, replace = TRUE),
    dv = c("Success", "Fail", "Borderline")  |> sample(100, replace = TRUE)
  ) |> count(iv, dv))
#> # A tibble: 12 × 3
#>    iv    dv             n
#>    <chr> <chr>      <int>
#>  1 a     Borderline    14
#>  2 a     Fail           8
#>  3 a     Success       13
#>  4 b     Borderline     7
#>  5 b     Fail           9
#>  6 b     Success        7
#>  7 c     Borderline     7
#>  8 c     Fail           1
#>  9 c     Success        8
#> 10 d     Borderline     9
#> 11 d     Fail           5
#> 12 d     Success       12

d |> contingency_table(dv, .wt = n)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 4 × 4
#>   iv    Borderline  Fail Success
#> * <chr>      <int> <int>   <int>
#> 1 a             14     8      13
#> 2 b              7     9       7
#> 3 c              7     1       8
#> 4 d              9     5      12

d |> contingency_table(dv, .wt = n, .rownames = TRUE) |>
    print_lf() |>
    chisq.test()
#>   Borderline Fail Success
#> a         14    8      13
#> b          7    9       7
#> c          7    1       8
#> d          9    5      12
#> 
#> Warning: Chi-squared approximation may be incorrect
#> 
#>  Pearson's Chi-squared test
#> 
#> data:  print_lf(contingency_table(d, dv, .wt = n, .rownames = TRUE))
#> X-squared = 6.5483, df = 6, p-value = 0.3646
#> 

## Use .data pronoun for more informative error messages
d |> contingency_table(dv, .wt = .data$n)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 4 × 4
#>   iv    Borderline  Fail Success
#> * <chr>      <int> <int>   <int>
#> 1 a             14     8      13
#> 2 b              7     9       7
#> 3 c              7     1       8
#> 4 d              9     5      12

try(d |> contingency_table(dv, .wt = .data$x))
#> Error in .data$x : Column `x` not found in `.data`.

rm(d)

## Using gss_cat dataset from {forcats} package

gss_cat |> contingency_table(race, relig, denom)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 47 × 5
#>    relig              denom             White Black Other
#>  * <fct>              <fct>             <int> <int> <int>
#>  1 Protestant         Southern baptist   1151   355    30
#>  2 Protestant         Baptist-dk which    723   697    37
#>  3 Protestant         No denomination    1020   149    55
#>  4 Orthodox-christian Not applicable       92     2     1
#>  5 None               Not applicable     2816   384   323
#>  6 Christian          Not applicable      147    49    28
#>  7 Protestant         Lutheran-mo synod   208     2     2
#>  8 Protestant         Other              1886   468   180
#>  9 Protestant         United methodist   1007    49    11
#> 10 Jewish             Not applicable      370    10     8
#> # ℹ 37 more rows

gss_cat |> contingency_table(race, !c(marital, rincome:partyid))
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 47 × 5
#>    relig              denom             White Black Other
#>  * <fct>              <fct>             <int> <int> <int>
#>  1 Protestant         Southern baptist   1151   355    30
#>  2 Protestant         Baptist-dk which    723   697    37
#>  3 Protestant         No denomination    1020   149    55
#>  4 Orthodox-christian Not applicable       92     2     1
#>  5 None               Not applicable     2816   384   323
#>  6 Christian          Not applicable      147    49    28
#>  7 Protestant         Lutheran-mo synod   208     2     2
#>  8 Protestant         Other              1886   468   180
#>  9 Protestant         United methodist   1007    49    11
#> 10 Jewish             Not applicable      370    10     8
#> # ℹ 37 more rows

## Invokes warning and error message about duplicate 'row.names'
try(gss_cat |> contingency_table(race, relig, denom, .rownames = TRUE)) 
#> Warning: non-unique values when setting 'row.names': ‘Christian’, ‘Other’, ‘Protestant’
#> Error in `.rowNamesDF<-`(x, value = value) : 
#>   duplicate 'row.names' are not allowed

## Using xcontingency_table() avoids warning and error
gss_cat |> xcontingency_table(race, relig, denom)
#> ____________________________
#> Crossed Contingency Table: -
#> 
#> # A tibble: 47 × 4
#>    relig_denom                       White Black Other
#>  * <fct>                             <int> <int> <int>
#>  1 Protestant:Southern baptist        1151   355    30
#>  2 Protestant:Baptist-dk which         723   697    37
#>  3 Protestant:No denomination         1020   149    55
#>  4 Orthodox-christian:Not applicable    92     2     1
#>  5 None:Not applicable                2816   384   323
#>  6 Christian:Not applicable            147    49    28
#>  7 Protestant:Lutheran-mo synod        208     2     2
#>  8 Protestant:Other                   1886   468   180
#>  9 Protestant:United methodist        1007    49    11
#> 10 Jewish:Not applicable               370    10     8
#> # ℹ 37 more rows

gss_cat |> xcontingency_table(race, !c(marital, rincome:partyid))
#> ____________________________
#> Crossed Contingency Table: -
#> 
#> # A tibble: 47 × 4
#>    relig_denom                       White Black Other
#>  * <fct>                             <int> <int> <int>
#>  1 Protestant:Southern baptist        1151   355    30
#>  2 Protestant:Baptist-dk which         723   697    37
#>  3 Protestant:No denomination         1020   149    55
#>  4 Orthodox-christian:Not applicable    92     2     1
#>  5 None:Not applicable                2816   384   323
#>  6 Christian:Not applicable            147    49    28
#>  7 Protestant:Lutheran-mo synod        208     2     2
#>  8 Protestant:Other                   1886   468   180
#>  9 Protestant:United methodist        1007    49    11
#> 10 Jewish:Not applicable               370    10     8
#> # ℹ 37 more rows

gss_cat |> xcontingency_table(race, relig, denom, .crossname = "Denomination")
#> ____________________________
#> Crossed Contingency Table: -
#> 
#> # A tibble: 47 × 4
#>    Denomination                      White Black Other
#>  * <fct>                             <int> <int> <int>
#>  1 Protestant:Southern baptist        1151   355    30
#>  2 Protestant:Baptist-dk which         723   697    37
#>  3 Protestant:No denomination         1020   149    55
#>  4 Orthodox-christian:Not applicable    92     2     1
#>  5 None:Not applicable                2816   384   323
#>  6 Christian:Not applicable            147    49    28
#>  7 Protestant:Lutheran-mo synod        208     2     2
#>  8 Protestant:Other                   1886   468   180
#>  9 Protestant:United methodist        1007    49    11
#> 10 Jewish:Not applicable               370    10     8
#> # ℹ 37 more rows

gss_cat |>
    xcontingency_table(race, relig, denom, .rownames = TRUE) |>
    head(10)
#>                                   White Black Other
#> Protestant:Southern baptist        1151   355    30
#> Protestant:Baptist-dk which         723   697    37
#> Protestant:No denomination         1020   149    55
#> Orthodox-christian:Not applicable    92     2     1
#> None:Not applicable                2816   384   323
#> Christian:Not applicable            147    49    28
#> Protestant:Lutheran-mo synod        208     2     2
#> Protestant:Other                   1886   468   180
#> Protestant:United methodist        1007    49    11
#> Jewish:Not applicable               370    10     8

## Two more esoteric examples
ivars <- exprs(relig, denom)
gss_cat |> contingency_table(race, !!!ivars)
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 47 × 5
#>    relig              denom             White Black Other
#>  * <fct>              <fct>             <int> <int> <int>
#>  1 Protestant         Southern baptist   1151   355    30
#>  2 Protestant         Baptist-dk which    723   697    37
#>  3 Protestant         No denomination    1020   149    55
#>  4 Orthodox-christian Not applicable       92     2     1
#>  5 None               Not applicable     2816   384   323
#>  6 Christian          Not applicable      147    49    28
#>  7 Protestant         Lutheran-mo synod   208     2     2
#>  8 Protestant         Other              1886   468   180
#>  9 Protestant         United methodist   1007    49    11
#> 10 Jewish             Not applicable      370    10     8
#> # ℹ 37 more rows

ivars <- c("relig", "denom")
gss_cat |> contingency_table(race, any_of(ivars))
#> ____________________
#> Contingency Table: -
#> 
#> # A tibble: 47 × 5
#>    relig              denom             White Black Other
#>  * <fct>              <fct>             <int> <int> <int>
#>  1 Protestant         Southern baptist   1151   355    30
#>  2 Protestant         Baptist-dk which    723   697    37
#>  3 Protestant         No denomination    1020   149    55
#>  4 Orthodox-christian Not applicable       92     2     1
#>  5 None               Not applicable     2816   384   323
#>  6 Christian          Not applicable      147    49    28
#>  7 Protestant         Lutheran-mo synod   208     2     2
#>  8 Protestant         Other              1886   468   180
#>  9 Protestant         United methodist   1007    49    11
#> 10 Jewish             Not applicable      370    10     8
#> # ℹ 37 more rows

rm(ivars)

```

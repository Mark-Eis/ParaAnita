# Get and Set Contrasts Matrix for an Independent Variable in Data

`get_contrasts()` returns the `"contrasts"` attribute a selected factor
within a data frame.

`set_contrasts()` sets the `"contrasts"` attribute for a selected factor
within a data frame;

`set_contrasts()<-` is the replacement function form.

## Usage

``` r
get_contrasts(data, .f)

set_contrasts(data, .f, how.many = NULL, ..., contr)

set_contrasts(data, .f, how.many = NULL, ...) <- value
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

- .f:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  quoted name of a `factor` in `data`.

- how.many:

  the number of contrasts to set, by default one less than
  `nlevels(object)`.

- ...:

  additional arguments for the function `contr`.

- contr:

  which contrasts to use. Can be a matrix with one row for each level of
  the factor or a suitable function like `contr.poly` or a character
  string giving the name of the function

- value:

  either a numeric matrix (or a sparse or dense matrix of a class
  extending
  [`dMatrix`](https://rdrr.io/pkg/Matrix/man/dMatrix-class.html) from
  package [Matrix](https://CRAN.R-project.org/package=Matrix)) whose
  columns give coefficients for contrasts in the levels of `x`, or (the
  quoted name of) a function which computes such matrices.

## Value

A dataframe or
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
with the `"contrasts"` attribute set for `.f`.

## Details

The `"contrasts"` attribute of `.f` may be set using either a numeric
matrix or (the quoted name of) a function which computes such matrices,
supplied to `set_contrasts()` using the `contr` argument or the `value`
argument in the case of the replacement function form
`set_contrasts()<-`. A suitable contrast matrix may be obtained using a
contrast function such as `contr.helmert`, `contr.poly`, `contr.sum`,
`contr.treatment` or `contr.SAS`, or the (quoted) name of the function
itself may be supplied. Additional arguments, such as `base = ``x`, may
be supplied to a contrast function using the `...` argument of
`set_contrasts()` or `set_contrasts()<-`.

If a `base` argument is supplied when `contr = contr.treatment`, its
value is capped to be no greater than `nlevels(.f)`, hence it can be
specified as a large integer (e.g., `99L`) to ensure the last level is
the reference level. This may be convenient when using `set_contrasts()`
programmatically.

If `NULL` is supplied as the `contr` or `value` argument, any existing
`"contrasts"` attribute will be removed from `.f`.

## See also

`contrast`, [`contrasts`](https://rdrr.io/r/stats/contrasts.html), `C`,
`contr.helmert`, `contr.poly`, `contr.sum`, `contr.treatment` or
`contr.SAS`.

Other get-contrasts:
[`get_contr_data()`](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.md)

## Examples

``` r
## Create data frame with a factor iv
(d <- binom_data())
#> __________________________
#> Simulated Binomial Data: -
#> 
#> # A tibble: 5 × 3
#>   iv       pn    qn
#> * <fct> <int> <int>
#> 1 a        37    29
#> 2 b        24    42
#> 3 c        21    45
#> 4 d        13    53
#> 5 e         5    61

## set_contrasts()
d |> set_contrasts(iv, contr = contr.helmert) |> get_contrasts(iv)
#>   [,1] [,2] [,3] [,4]
#> a   -1   -1   -1   -1
#> b    1   -1   -1   -1
#> c    0    2   -1   -1
#> d    0    0    3   -1
#> e    0    0    0    4

d |> set_contrasts(iv, contr = contr.poly) |> get_contrasts(iv)
#>           .L         .Q         .C         ^4
#> a -0.6324555  0.5345225 -0.3162278  0.1195229
#> b -0.3162278 -0.2672612  0.6324555 -0.4780914
#> c  0.0000000 -0.5345225  0.0000000  0.7171372
#> d  0.3162278 -0.2672612 -0.6324555 -0.4780914
#> e  0.6324555  0.5345225  0.3162278  0.1195229

d |> set_contrasts(iv, contr = contr.sum) |> get_contrasts(iv)
#>   [,1] [,2] [,3] [,4]
#> a    1    0    0    0
#> b    0    1    0    0
#> c    0    0    1    0
#> d    0    0    0    1
#> e   -1   -1   -1   -1

d |> set_contrasts(iv, contr = contr.treatment) |> get_contrasts(iv)
#>   b c d e
#> a 0 0 0 0
#> b 1 0 0 0
#> c 0 1 0 0
#> d 0 0 1 0
#> e 0 0 0 1

d |> set_contrasts(iv, contr = contr.SAS) |> get_contrasts(iv)
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 0 0 0 1
#> e 0 0 0 0

## how.many argument
d |> set_contrasts(iv, 3, contr = contr.poly) |> get_contrasts(iv)
#>           .L         .Q         .C
#> a -0.6324555  0.5345225 -0.3162278
#> b -0.3162278 -0.2672612  0.6324555
#> c  0.0000000 -0.5345225  0.0000000
#> d  0.3162278 -0.2672612 -0.6324555
#> e  0.6324555  0.5345225  0.3162278

## base argument of contr.treatment
d |> set_contrasts(iv, base = 1, contr = contr.treatment) |> get_contrasts(iv)
#>   b c d e
#> a 0 0 0 0
#> b 1 0 0 0
#> c 0 1 0 0
#> d 0 0 1 0
#> e 0 0 0 1

d |> set_contrasts(iv, base = 3, contr = contr.treatment) |> get_contrasts(iv)
#>   a b d e
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 0 0
#> d 0 0 1 0
#> e 0 0 0 1

## base argument of contr.treatment limited to nlevels(d$iv) 
d |> set_contrasts(iv, base = 99L, contr = contr.treatment) |> get_contrasts(iv)
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 0 0 0 1
#> e 0 0 0 0

## Remove "contrasts" attribute using NULL
d |> set_contrasts(iv, contr = NULL) |> get_contrasts(iv)
#> NULL

## set_contrasts()<- replacement form
set_contrasts(d, iv) <- contr.helmert
d |> get_contrasts(iv)
#>   [,1] [,2] [,3] [,4]
#> a   -1   -1   -1   -1
#> b    1   -1   -1   -1
#> c    0    2   -1   -1
#> d    0    0    3   -1
#> e    0    0    0    4

set_contrasts(d, iv) <- contr.poly
d |> get_contrasts(iv)
#>           .L         .Q         .C         ^4
#> a -0.6324555  0.5345225 -0.3162278  0.1195229
#> b -0.3162278 -0.2672612  0.6324555 -0.4780914
#> c  0.0000000 -0.5345225  0.0000000  0.7171372
#> d  0.3162278 -0.2672612 -0.6324555 -0.4780914
#> e  0.6324555  0.5345225  0.3162278  0.1195229

set_contrasts(d, iv) <- contr.sum
d |> get_contrasts(iv)
#>   [,1] [,2] [,3] [,4]
#> a    1    0    0    0
#> b    0    1    0    0
#> c    0    0    1    0
#> d    0    0    0    1
#> e   -1   -1   -1   -1

set_contrasts(d, iv) <- contr.treatment
d |> get_contrasts(iv)
#>   b c d e
#> a 0 0 0 0
#> b 1 0 0 0
#> c 0 1 0 0
#> d 0 0 1 0
#> e 0 0 0 1

set_contrasts(d, iv) <- contr.SAS
d |> get_contrasts(iv)
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 0 0 0 1
#> e 0 0 0 0

## how.many argument
set_contrasts(d, iv, 3) <- contr.poly
d |> get_contrasts(iv)
#>           .L         .Q         .C
#> a -0.6324555  0.5345225 -0.3162278
#> b -0.3162278 -0.2672612  0.6324555
#> c  0.0000000 -0.5345225  0.0000000
#> d  0.3162278 -0.2672612 -0.6324555
#> e  0.6324555  0.5345225  0.3162278

## base argument of contr.treatment
set_contrasts(d, iv, base = 2) <- contr.treatment
d |> get_contrasts(iv)
#>   a c d e
#> a 1 0 0 0
#> b 0 0 0 0
#> c 0 1 0 0
#> d 0 0 1 0
#> e 0 0 0 1

set_contrasts(d, iv, base = 4) <- contr.treatment
d |> get_contrasts(iv)
#>   a b c e
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 0 0 0 0
#> e 0 0 0 1

## base argument of contr.treatment limited to nlevels(d$iv) 
set_contrasts(d, iv, base = 99L) <- contr.treatment
d |> get_contrasts(iv)
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 0 0 0 1
#> e 0 0 0 0

rm(d)
```

# Create and Set Names for Helmert Contrasts

Create and set column names for Helmert contrasts associated with a
factor.

## Usage

``` r
helm_names(x)

helm_names(x) <- value
```

## Arguments

- x:

  a `factor` for which the contrast column headings are to be named.

- value:

  a character vector or [`list`](https://rdrr.io/r/base/list.html) of
  length `two` (any subsequent elements will be ignored), which are
  separators used in creating the headings; the first is a "within"
  separator and the second a "between" separator, see examples.

## Details

Helmert contrasts for a factor contrast the second level with the first,
the third with the average of the first two, and so on. A contrasts
matrix can be set as an attribute of a factor using the function
[`contrasts<-`](https://rdrr.io/r/stats/contrasts.html), and using the
contrasts function `contr.helmert` as an argument i.e.,
`contrasts(f)<- contr.helmert`, will create and set an appropriate
Helmert contrasts matrix attribute. However the columns of the resulting
Helmert contrasts matrix are unnamed. `helm_names()<-` creates these
column names based on the names the levels of the factor `x`.

If factor `x` does not have an associated Helmert contrast matrix,
`helm_names()<-` will set one, giving a message. The names of the levels
of factor `x` need to be fairly short for function this to be helpful.

The function `helm_names()` is an alias for
[`contr_colnames`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.md)
and included simply for completeness and to optimise help searches.

## See also

`contrast`, [`contrasts`](https://rdrr.io/r/stats/contrasts.html) and
`contr.helmert`.

Other contrast-names:
[`contr_colnames()`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.md)

## Examples

``` r
(f <- gl(5, 5, labels = LETTERS[1:5]))
#>  [1] A A A A A B B B B B C C C C C D D D D D E E E E E
#> Levels: A B C D E

contrasts(f)<- contr.helmert
f
#>  [1] A A A A A B B B B B C C C C C D D D D D E E E E E
#> attr(,"contrasts")
#>   [,1] [,2] [,3] [,4]
#> A   -1   -1   -1   -1
#> B    1   -1   -1   -1
#> C    0    2   -1   -1
#> D    0    0    3   -1
#> E    0    0    0    4
#> Levels: A B C D E
helm_names(f)
#> NULL

helm_names(f)<- c(":", "v")
f
#>  [1] A A A A A B B B B B C C C C C D D D D D E E E E E
#> attr(,"contrasts")
#>   A v B A:B v C A:C v D A:D v E
#> A    -1      -1      -1      -1
#> B     1      -1      -1      -1
#> C     0       2      -1      -1
#> D     0       0       3      -1
#> E     0       0       0       4
#> Levels: A B C D E
helm_names(f)
#> [1] "A v B"   "A:B v C" "A:C v D" "A:D v E"

contrasts(f)<- NULL
f
#>  [1] A A A A A B B B B B C C C C C D D D D D E E E E E
#> Levels: A B C D E

helm_names(f)<- c("-", "vs.")
#> Setting Helmert contrasts for factor x with `helm_names()<-`.
f
#>  [1] A A A A A B B B B B C C C C C D D D D D E E E E E
#> attr(,"contrasts")
#>   A vs. B A-B vs. C A-C vs. D A-D vs. E
#> A      -1        -1        -1        -1
#> B       1        -1        -1        -1
#> C       0         2        -1        -1
#> D       0         0         3        -1
#> E       0         0         0         4
#> Levels: A B C D E
helm_names(f)
#> [1] "A vs. B"   "A-B vs. C" "A-C vs. D" "A-D vs. E"

rm(f)
```

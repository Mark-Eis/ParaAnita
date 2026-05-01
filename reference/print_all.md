# Print All (or More) of an Object

`print_all()` is a generic function for extended printing of an object,
for instance printing all rows of a tibble, a derived class or even a
regular data frame, optionally following up by printing a specified
number of linefeeds. Being a generic function, new printing methods can
be easily added for a new [`class`](https://rdrr.io/r/base/class.html).

## Usage

``` r
print_all(x, ...)

# S3 method for class 'data.frame'
print_all(
  x,
  linefeeds = NULL,
  ...,
  digits = NULL,
  quote = FALSE,
  right = TRUE,
  row.names = TRUE,
  max = NULL
)

# S3 method for class 'tbl'
print_all(
  x,
  linefeeds = NULL,
  width = NULL,
  ...,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)

# S3 method for class 'tbl_df'
print_all(
  x,
  linefeeds = NULL,
  width = NULL,
  ...,
  max_extra_cols = NULL,
  max_footer_lines = NULL
)

# S3 method for class 'odds_ratio'
print_all(x, linefeeds = NULL, ...)

# S3 method for class 'htest'
print_all(x, ...)
```

## Arguments

- x:

  An object such as a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
  data frame.

- ...:

  further arguments passed to or from other methods.

- linefeeds:

  A positive integer specifying the number of linefeeds to follow up the
  printed output; default `NULL`.

- digits:

  minimal number of *significant* digits, see
  [`print.default`](https://rdrr.io/r/base/print.default.html).

- quote:

  logical, indicating whether or not strings should be printed with
  surrounding quotes.

- right:

  logical, indicating whether or not strings should be right aligned.

- row.names:

  logical (or character vector), indicating whether (or what) row names
  should be printed.

- max:

  numeric or `NULL`, specifying the maximal number of entries to be
  printed. By default, when `NULL`,
  [`getOption`](https://rdrr.io/r/base/options.html)`("max.print")`
  used.

- width:

  only used when `max.levels` is NULL, see above.

- max_extra_cols:

  Number of extra columns to print abbreviated information for, if the
  width is too small for the entire tibble. If `NULL`, the
  `max_extra_cols`
  [option](https://pillar.r-lib.org/reference/pillar_options.html) is
  used. The previously defined `n_extra` argument is soft-deprecated.

- max_footer_lines:

  Maximum number of footer lines. If `NULL`, the `max_footer_lines`
  [option](https://pillar.r-lib.org/reference/pillar_options.html) is
  used.

## Value

Invisibly returns its argument.

## Details

For a
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)
`x`, `print_all(x)` is equivalent to `print(x, n = nrow(x))`, followed
up if required by `n` linefeeds generated as if by using
`cat(rep("\n", n))`.

The `linefeeds` argument may be useful within a piped sequence to
separate output from subsequent printing. If a vector of `length > 1` is
entered for `linefeeds`, only the first element will be used, and
negative integers will be converted to zero i.e., no line feeds.

## See also

[`print()`](https://rdrr.io/r/base/print.html),
[`print_lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md),
[`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html).

Other print:
[`Print_Methods`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md),
[`announce()`](https://mark-eis.github.io/ParaAnita/reference/announce.md),
[`lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md)

## Examples

``` r
(tib <- tibble(x = 1:26, y = LETTERS[x], z = paste0(x, y)))
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> # ℹ 16 more rows
tib |> print_all()
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> 11    11 K     11K  
#> 12    12 L     12L  
#> 13    13 M     13M  
#> 14    14 N     14N  
#> 15    15 O     15O  
#> 16    16 P     16P  
#> 17    17 Q     17Q  
#> 18    18 R     18R  
#> 19    19 S     19S  
#> 20    20 T     20T  
#> 21    21 U     21U  
#> 22    22 V     22V  
#> 23    23 W     23W  
#> 24    24 X     24X  
#> 25    25 Y     25Y  
#> 26    26 Z     26Z  
tib |> print_all() |> names()
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> 11    11 K     11K  
#> 12    12 L     12L  
#> 13    13 M     13M  
#> 14    14 N     14N  
#> 15    15 O     15O  
#> 16    16 P     16P  
#> 17    17 Q     17Q  
#> 18    18 R     18R  
#> 19    19 S     19S  
#> 20    20 T     20T  
#> 21    21 U     21U  
#> 22    22 V     22V  
#> 23    23 W     23W  
#> 24    24 X     24X  
#> 25    25 Y     25Y  
#> 26    26 Z     26Z  
#> [1] "x" "y" "z"
tib |> print_all(linefeeds = 3) |> names()
#> # A tibble: 26 × 3
#>        x y     z    
#>    <int> <chr> <chr>
#>  1     1 A     1A   
#>  2     2 B     2B   
#>  3     3 C     3C   
#>  4     4 D     4D   
#>  5     5 E     5E   
#>  6     6 F     6F   
#>  7     7 G     7G   
#>  8     8 H     8H   
#>  9     9 I     9I   
#> 10    10 J     10J  
#> 11    11 K     11K  
#> 12    12 L     12L  
#> 13    13 M     13M  
#> 14    14 N     14N  
#> 15    15 O     15O  
#> 16    16 P     16P  
#> 17    17 Q     17Q  
#> 18    18 R     18R  
#> 19    19 S     19S  
#> 20    20 T     20T  
#> 21    21 U     21U  
#> 22    22 V     22V  
#> 23    23 W     23W  
#> 24    24 X     24X  
#> 25    25 Y     25Y  
#> 26    26 Z     26Z  
#> 
#>  
#>  
#> [1] "x" "y" "z"

df <- tib |> as.data.frame()
df |> print_all()                         ## Does nothing more than regular print()
#>     x y   z
#> 1   1 A  1A
#> 2   2 B  2B
#> 3   3 C  3C
#> 4   4 D  4D
#> 5   5 E  5E
#> 6   6 F  6F
#> 7   7 G  7G
#> 8   8 H  8H
#> 9   9 I  9I
#> 10 10 J 10J
#> 11 11 K 11K
#> 12 12 L 12L
#> 13 13 M 13M
#> 14 14 N 14N
#> 15 15 O 15O
#> 16 16 P 16P
#> 17 17 Q 17Q
#> 18 18 R 18R
#> 19 19 S 19S
#> 20 20 T 20T
#> 21 21 U 21U
#> 22 22 V 22V
#> 23 23 W 23W
#> 24 24 X 24X
#> 25 25 Y 25Y
#> 26 26 Z 26Z
df |> print_all(linefeeds = 2) |> names() ## Regular data frame printing, with line feeds
#>     x y   z
#> 1   1 A  1A
#> 2   2 B  2B
#> 3   3 C  3C
#> 4   4 D  4D
#> 5   5 E  5E
#> 6   6 F  6F
#> 7   7 G  7G
#> 8   8 H  8H
#> 9   9 I  9I
#> 10 10 J 10J
#> 11 11 K 11K
#> 12 12 L 12L
#> 13 13 M 13M
#> 14 14 N 14N
#> 15 15 O 15O
#> 16 16 P 16P
#> 17 17 Q 17Q
#> 18 18 R 18R
#> 19 19 S 19S
#> 20 20 T 20T
#> 21 21 U 21U
#> 22 22 V 22V
#> 23 23 W 23W
#> 24 24 X 24X
#> 25 25 Y 25Y
#> 26 26 Z 26Z
#> 
#>  
#> [1] "x" "y" "z"

binom_data(26, 100) |>
    odds_ratio(.dep_var = cbind(pn, qn), .ind_var = iv) |>
    print_all()
#> Waiting for profiling to be done...
#> ____________________________
#> Estimates and Odds Ratios: -
#> 
#> # A tibble: 26 × 7
#>    parameter   estimate    se     p_val odds_ratio ci[,"2.5%"] [,"97.5%"] sig  
#>    <chr>          <dbl> <dbl>     <dbl>      <dbl>       <dbl>      <dbl> <fct>
#>  1 (Intercept)   0.0400 0.200 0.841         1          NA          NA     NS   
#>  2 ivb           0.120  0.283 0.671         1.13        0.647       1.97  NS   
#>  3 ivc          -0.0400 0.283 0.888         0.961       0.551       1.67  NS   
#>  4 ivd          -0.120  0.283 0.671         0.887       0.508       1.54  NS   
#>  5 ive          -0.530  0.287 0.0652        0.589       0.334       1.03  .    
#>  6 ivf          -0.794  0.293 0.00679       0.452       0.253       0.799 **   
#>  7 ivg          -0.363  0.285 0.203         0.696       0.397       1.21  NS   
#>  8 ivh          -0.794  0.293 0.00679       0.452       0.253       0.799 **   
#>  9 ivi          -0.160  0.283 0.572         0.852       0.488       1.48  NS   
#> 10 ivj          -0.572  0.288 0.0469        0.564       0.319       0.989 *    
#> 11 ivk          -0.703  0.291 0.0156        0.495       0.278       0.872 *    
#> 12 ivl          -0.659  0.290 0.0229        0.517       0.291       0.910 *    
#> 13 ivm          -1.25   0.311 0.0000585     0.287       0.154       0.522 ***  
#> 14 ivn          -0.794  0.293 0.00679       0.452       0.253       0.799 **   
#> 15 ivo          -0.887  0.296 0.00272       0.412       0.229       0.731 **   
#> 16 ivp          -0.984  0.299 0.00101       0.374       0.206       0.667 **   
#> 17 ivq          -1.09   0.303 0.000343      0.338       0.184       0.607 ***  
#> 18 ivr          -1.03   0.301 0.000594      0.355       0.195       0.637 ***  
#> 19 ivs          -1.36   0.317 0.0000163     0.255       0.135       0.469 ***  
#> 20 ivt          -1.36   0.317 0.0000163     0.255       0.135       0.469 ***  
#> 21 ivu          -1.77   0.344 0.0000003     0.170       0.0841      0.326 ***  
#> 22 ivv          -1.70   0.338 0.0000005     0.183       0.0920      0.349 ***  
#> 23 ivw          -1.49   0.324 0.0000043     0.225       0.117       0.419 ***  
#> 24 ivx          -2.03   0.367 0             0.131       0.0615      0.261 ***  
#> 25 ivy          -2.13   0.377 0             0.119       0.0543      0.241 ***  
#> 26 ivz          -2.79   0.466 0             0.0613      0.0223      0.143 ***  

rm(df, tib)
```

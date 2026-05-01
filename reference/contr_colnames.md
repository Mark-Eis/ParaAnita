# Get and Set Contrast Matrix Column Names

Set and view the column names of contrasts associated with a factor.

## Usage

``` r
contr_colnames(x)

contr_colnames(x) <- value

contr_colpfx(x) <- value
```

## Arguments

- x:

  a `factor` for which the contrast column headings are to be set or
  viewed.

- value:

  a character vector containing the contrast column names, of length
  equal to the number of contrasts; or in the case of `contr_colpfx()<-`
  a character or string used to prefix the existing contrast column
  names.

## Details

`contr_colnames()` returns the current column names of the contrasts for
a factor.

`contr_colnames()<-` sets the column names of the contrasts for a
factor.

`cntr_pfx()<-` prefixes the current column names of the contrasts for a
factor with the character or string provided. This can be useful when
factor levels are elided with the factor name as, for instance, in the
printed output of
[`summary.glm`](https://rdrr.io/r/stats/summary.glm.html).

If contrasts are not set for `x`, both `contr_colnames()<-` and
`cntr_pfx()<-` set the contrast attribute using the default function
from [`options`](https://rdrr.io/r/base/options.html)`("contrasts")`
before modifying the column names.

## See also

`contrast`, [`contrasts`](https://rdrr.io/r/stats/contrasts.html) and
[`factor`](https://rdrr.io/r/base/factor.html).

Other contrast-names:
[`helm_names()`](https://mark-eis.github.io/ParaAnita/reference/helm_names.md)

## Examples

``` r
(d <- data.frame(
  f = gl(5, 5, labels = LETTERS[1:5]),
  dv = sample(c(0,1), 25, replace = TRUE)
))
#>    f dv
#> 1  A  1
#> 2  A  0
#> 3  A  1
#> 4  A  1
#> 5  A  1
#> 6  B  1
#> 7  B  0
#> 8  B  0
#> 9  B  0
#> 10 B  1
#> 11 C  0
#> 12 C  0
#> 13 C  1
#> 14 C  0
#> 15 C  1
#> 16 D  1
#> 17 D  1
#> 18 D  1
#> 19 D  1
#> 20 D  0
#> 21 E  1
#> 22 E  0
#> 23 E  1
#> 24 E  1
#> 25 E  0

contrasts(d$f) <- contr.helmert
contrasts(d$f)
#>   [,1] [,2] [,3] [,4]
#> A   -1   -1   -1   -1
#> B    1   -1   -1   -1
#> C    0    2   -1   -1
#> D    0    0    3   -1
#> E    0    0    0    4
contr_colnames(d$f)
#> NULL
glm(dv ~ f, family = binomial, data = d) |> summary()
#> 
#> Call:
#> glm(formula = dv ~ f, family = binomial, data = d)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)
#> (Intercept)  0.47342    0.44721   1.059    0.290
#> f1          -0.89588    0.72169  -1.241    0.214
#> f2          -0.29863    0.38790  -0.770    0.441
#> f3           0.29863    0.31366   0.952    0.341
#> f4          -0.01699    0.20916  -0.081    0.935
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 33.651  on 24  degrees of freedom
#> Residual deviance: 30.198  on 20  degrees of freedom
#> AIC: 40.198
#> 
#> Number of Fisher Scoring iterations: 4
#> 

contr_colnames(d$f) <- c("A v. B", "AB v. C", "ABC v. D", "ABCD v. E")
contr_colnames(d$f)
#> [1] "A v. B"    "AB v. C"   "ABC v. D"  "ABCD v. E"
contrasts(d$f)
#>   A v. B AB v. C ABC v. D ABCD v. E
#> A     -1      -1       -1        -1
#> B      1      -1       -1        -1
#> C      0       2       -1        -1
#> D      0       0        3        -1
#> E      0       0        0         4
glm(dv ~ f, family = binomial, data = d) |> summary()
#> 
#> Call:
#> glm(formula = dv ~ f, family = binomial, data = d)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)
#> (Intercept)  0.47342    0.44721   1.059    0.290
#> fA v. B     -0.89588    0.72169  -1.241    0.214
#> fAB v. C    -0.29863    0.38790  -0.770    0.441
#> fABC v. D    0.29863    0.31366   0.952    0.341
#> fABCD v. E  -0.01699    0.20916  -0.081    0.935
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 33.651  on 24  degrees of freedom
#> Residual deviance: 30.198  on 20  degrees of freedom
#> AIC: 40.198
#> 
#> Number of Fisher Scoring iterations: 4
#> 

contr_colpfx(d$f) <- ": "
contr_colnames(d$f)
#> [1] ": A v. B"    ": AB v. C"   ": ABC v. D"  ": ABCD v. E"
glm(dv ~ f, family = binomial, data = d) |> summary()
#> 
#> Call:
#> glm(formula = dv ~ f, family = binomial, data = d)
#> 
#> Coefficients:
#>              Estimate Std. Error z value Pr(>|z|)
#> (Intercept)   0.47342    0.44721   1.059    0.290
#> f: A v. B    -0.89588    0.72169  -1.241    0.214
#> f: AB v. C   -0.29863    0.38790  -0.770    0.441
#> f: ABC v. D   0.29863    0.31366   0.952    0.341
#> f: ABCD v. E -0.01699    0.20916  -0.081    0.935
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 33.651  on 24  degrees of freedom
#> Residual deviance: 30.198  on 20  degrees of freedom
#> AIC: 40.198
#> 
#> Number of Fisher Scoring iterations: 4
#> 

rm(d)
```

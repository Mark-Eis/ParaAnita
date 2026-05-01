# Factor as Numeric

Transform a `factor` to approximately its original numeric values.

## Usage

``` r
fct_to_num(f)
```

## Arguments

- f:

  `factor` to be converted to numeric values

## Value

Numeric

## Details

See ‘Warning’ section of [`factor`](https://rdrr.io/r/base/factor.html):
“In particular, `as.numeric` applied to a factor is meaningless, and may
happen by implicit coercion. To transform a factor `f` to approximately
its original numeric values, `as.numeric(levels(f))[f]` is recommended
and slightly more efficient than `as.numeric(as.character(f))`.”

## See also

[`factor`](https://rdrr.io/r/base/factor.html)

Other factor-manip:
[`add_grps()`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md)

## Examples

``` r
f <- factor(2001:2020)

f
#>  [1] 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
#> [16] 2016 2017 2018 2019 2020
#> 20 Levels: 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 ... 2020

f |> as.numeric()  # Returns codes for factor levels, not what is required
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

f |> fct_to_num()  # Returns approximate numeric values, as required
#>  [1] 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
#> [16] 2016 2017 2018 2019 2020

rm(f)
```

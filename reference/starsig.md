# Stars for Statistical Significance

Stars for statistical significance with levels as usual in R. A
vectorised function.

## Usage

``` r
starsig(p)
```

## Arguments

- p:

  A numeric vector of probabilities.

## Value

A `character vector`, length of `p`.

## Examples

``` r
(test_seq <- c(0.0003, 0.0010, 0.0032, 0.0100, 0.0316, 0.0500, 0.0631, 0.1000, 0.3162))
#> [1] 0.0003 0.0010 0.0032 0.0100 0.0316 0.0500 0.0631 0.1000 0.3162
starsig(test_seq)
#> [1] *** **  **  *   *   .   .   NS  NS 
#> Levels: *** ** * . NS
rbind(test_seq, as.character(starsig(test_seq)))
#>          [,1]    [,2]    [,3]     [,4]   [,5]     [,6]   [,7]     [,8] 
#> test_seq "3e-04" "0.001" "0.0032" "0.01" "0.0316" "0.05" "0.0631" "0.1"
#>          "***"   "**"    "**"     "*"    "*"      "."    "."      "NS" 
#>          [,9]    
#> test_seq "0.3162"
#>          "NS"    
data.frame(val = test_seq, sig = starsig(test_seq))
#>      val sig
#> 1 0.0003 ***
#> 2 0.0010  **
#> 3 0.0032  **
#> 4 0.0100   *
#> 5 0.0316   *
#> 6 0.0500   .
#> 7 0.0631   .
#> 8 0.1000  NS
#> 9 0.3162  NS

rm(test_seq)
```

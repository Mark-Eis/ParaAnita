# Levels of all Factors in Data

`levels_data()` returns the levels for all factors in data.

`nlevels_data()` returns the number of levels for all factors in data.

## Usage

``` r
levels_data(data)

nlevels_data(data)
```

## Arguments

- data:

  a data frame, or a data frame extension (e.g. a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble-package.html)).

## Value

`levels_data()` returns a named `list` comprising the `levels` of each
`factor` in `data`. `nlevels_data()` returns a named `integer vector`
comprising the number of `levels` of each `factor` in `data`.

## Details

Does what it says on the tin.

## See also

[`levels`](https://rdrr.io/r/base/levels.html).

Other levels_data:
[`good_levels()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md)

## Examples

``` r
## Using gss_cat dataset from {forcats} package

gss_cat |> names()
#> [1] "year"    "marital" "age"     "race"    "rincome" "partyid" "relig"  
#> [8] "denom"   "tvhours"
gss_cat |> levels_data()
#> $marital
#> [1] "No answer"     "Never married" "Separated"     "Divorced"     
#> [5] "Widowed"       "Married"      
#> 
#> $race
#> [1] "Other"          "Black"          "White"          "Not applicable"
#> 
#> $rincome
#>  [1] "No answer"      "Don't know"     "Refused"        "$25000 or more"
#>  [5] "$20000 - 24999" "$15000 - 19999" "$10000 - 14999" "$8000 to 9999" 
#>  [9] "$7000 to 7999"  "$6000 to 6999"  "$5000 to 5999"  "$4000 to 4999" 
#> [13] "$3000 to 3999"  "$1000 to 2999"  "Lt $1000"       "Not applicable"
#> 
#> $partyid
#>  [1] "No answer"          "Don't know"         "Other party"       
#>  [4] "Strong republican"  "Not str republican" "Ind,near rep"      
#>  [7] "Independent"        "Ind,near dem"       "Not str democrat"  
#> [10] "Strong democrat"   
#> 
#> $relig
#>  [1] "No answer"               "Don't know"             
#>  [3] "Inter-nondenominational" "Native american"        
#>  [5] "Christian"               "Orthodox-christian"     
#>  [7] "Moslem/islam"            "Other eastern"          
#>  [9] "Hinduism"                "Buddhism"               
#> [11] "Other"                   "None"                   
#> [13] "Jewish"                  "Catholic"               
#> [15] "Protestant"              "Not applicable"         
#> 
#> $denom
#>  [1] "No answer"            "Don't know"           "No denomination"     
#>  [4] "Other"                "Episcopal"            "Presbyterian-dk wh"  
#>  [7] "Presbyterian, merged" "Other presbyterian"   "United pres ch in us"
#> [10] "Presbyterian c in us" "Lutheran-dk which"    "Evangelical luth"    
#> [13] "Other lutheran"       "Wi evan luth synod"   "Lutheran-mo synod"   
#> [16] "Luth ch in america"   "Am lutheran"          "Methodist-dk which"  
#> [19] "Other methodist"      "United methodist"     "Afr meth ep zion"    
#> [22] "Afr meth episcopal"   "Baptist-dk which"     "Other baptists"      
#> [25] "Southern baptist"     "Nat bapt conv usa"    "Nat bapt conv of am" 
#> [28] "Am bapt ch in usa"    "Am baptist asso"      "Not applicable"      
#> 
gss_cat |> nlevels_data()
#> marital    race rincome partyid   relig   denom 
#>       6       4      16      10      16      30 
```

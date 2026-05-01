# Announce Class for Consistent Printing

Creates an object of class `"announce"` with a built-in title string
used for printing.

## Usage

``` r
announce(object = vector(), lead = "Announce", ...)
```

## Arguments

- object:

  Object to be converted to `"announce"` class.

- lead:

  a `character` string giving the title to be printed.

- ...:

  other named arguments to be forwarded to print methods of classes
  inherited from `object`.

## Value

an object of class `"announce"` inheriting from class(es) of `object`.

## Details

`announce()` converts an object to class `"announce"`, inheriting from
its existing class(es).

## See also

[`cat()`](https://rdrr.io/r/base/cat.html),
[`print()`](https://rdrr.io/r/base/print.html).

Other print:
[`Print_Methods`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md),
[`lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md),
[`print_all()`](https://mark-eis.github.io/ParaAnita/reference/print_all.md)

## Examples

``` r
announce()
#> ___________
#> Announce: -
#> 
#> logical(0)
(cpt <- announce("x", lead = "Lorem ipsum dolor sit amet"))
#> _____________________________
#> Lorem ipsum dolor sit amet: -
#> 
#> [1] "x"
.class2(cpt)
#> [1] "announce"  "character"

## an Announce object, or one inheriting from announce, can be safely overwritten
(cpt <- announce(cpt, "Consectetur adipiscing elit"))
#> ______________________________
#> Consectetur adipiscing elit: -
#> 
#> [1] "x"
.class2(cpt)
#> [1] "announce"  "character"

rm(cpt)
```

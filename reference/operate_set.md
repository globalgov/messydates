# Set operations for messy dates

Performs intersection (`%intersect%`) and union (`%union%`) on the dates
or date-times implied by messy date class objects, treating each as the
(day-granularity) set of dates it expands to. Both return a plain
character vector of the individual member dates.

For a union that instead returns an `mdate` object in its most succinct
(contracted) notation, e.g. a range rather than a list of every day
within it, use `+` (see
[`?operate_arithmetic`](https://globalgov.github.io/messydates/reference/operate_arithmetic.md))
instead.

## Usage

``` r
e1 %intersect% e2

# S3 method for class 'mdate'
e1 %intersect% e2

e1 %union% e2

# S3 method for class 'mdate'
e1 %union% e2
```

## Arguments

- e1, e2:

  Messy date or other class objects

## Value

A vector of the same mode for `%intersect%`, or a common mode for
`%union%`.

## Details

Both kinds of set expand to the same members, so these operators return
the same dates for [`{}`](https://rdrr.io/r/base/Paren.html) ("all
members of") and `[]` ("one member of") alike. What differs is the
reading: for a [`{}`](https://rdrr.io/r/base/Paren.html) set the result
lists dates that are shared with (or covered by) the other operand,
whereas for a `[]` set it lists the *candidates* that remain possible,
so an empty intersection rules the other operand out rather than showing
no overlap.

## Functions

- ` %intersect% `: Find intersection of sets of messy dates

- ` %union% `: Find union of sets of messy dates

## Examples

``` r
as_messydate("2012-01-01..2012-01-20") %intersect% as_messydate("2012-01")
#>  [1] "2012-01-01" "2012-01-02" "2012-01-03" "2012-01-04" "2012-01-05"
#>  [6] "2012-01-06" "2012-01-07" "2012-01-08" "2012-01-09" "2012-01-10"
#> [11] "2012-01-11" "2012-01-12" "2012-01-13" "2012-01-14" "2012-01-15"
#> [16] "2012-01-16" "2012-01-17" "2012-01-18" "2012-01-19" "2012-01-20"
as_messydate("2012-01-01..2012-01-20") %union% as_messydate("2012-01")
#>  [1] "2012-01-01" "2012-01-02" "2012-01-03" "2012-01-04" "2012-01-05"
#>  [6] "2012-01-06" "2012-01-07" "2012-01-08" "2012-01-09" "2012-01-10"
#> [11] "2012-01-11" "2012-01-12" "2012-01-13" "2012-01-14" "2012-01-15"
#> [16] "2012-01-16" "2012-01-17" "2012-01-18" "2012-01-19" "2012-01-20"
#> [21] "2012-01-21" "2012-01-22" "2012-01-23" "2012-01-24" "2012-01-25"
#> [26] "2012-01-26" "2012-01-27" "2012-01-28" "2012-01-29" "2012-01-30"
#> [31] "2012-01-31"
```

# Contract lists of dates into messy dates

This function operates as the opposite of
[`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md).
It contracts a list of dates into the abbreviated annotation of messy
dates.

## Usage

``` r
contract(x, collapse = TRUE)
```

## Arguments

- x:

  A list of dates

- collapse:

  Do you want ranges to be collapsed? TRUE by default. If FALSE ranges
  are returned in compact format.

## Value

A `mdate` vector

## Details

The ´contract()´ function first
[`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
'mdate' objects to then display their most succinct representation.

Because
[`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
drops the time of day from ranges (see
[`?expand`](https://globalgov.github.io/messydates/reference/convert_expand.md)),
contracting a date-time range and then re-expanding it will not restore
the original times; `contract()` is intended for date-level ranges,
sets, and unspecified components.

## Examples

``` r
d <- as_messydate(c("2001-01-01", "2001-01", "2001",
"2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
"{2001-01,2001-02-02}", "28 BC", "-2000-01-01",
"{2001-01-01, 2001-01-02, 2001-01-03}"))
data.frame(d, contracted = contract(d))
#>                                    d              contracted
#> 1                         2001-01-01              2001-01-01
#> 2                            2001-01                 2001-01
#> 3                               2001                    2001
#> 4             2001-01-01..2001-02-02  2001-01-01..2001-02-02
#> 5            {2001-10-01,2001-10-04} {2001-10-01,2001-10-04}
#> 6               {2001-01,2001-02-02}    {2001-01,2001-02-02}
#> 7                              -0028                   -0028
#> 8                        -2000-01-01             -2000-01-01
#> 9 {2001-01-01,2001-01-02,2001-01-03}  2001-01-01..2001-01-03
# a full-month range collapses to a year-month by default...
contract(as_messydate("2012-06-01..2012-06-30"))
#>  'mdate' chr "2012-06"
# ...unless collapse = FALSE keeps it as an explicit start..end range
contract(as_messydate("2012-06-01..2012-06-30"), collapse = FALSE)
#>  'mdate' chr "2012-06-01..2012-06-30"
```

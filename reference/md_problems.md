# Diagnose unparseable dates

Reports which elements of a vector
[`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
cannot represent, and why. Where
[`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
stops at the first problem, or returns `NA` for text it could not read,
`md_problems()` inspects every element and returns one row per failure.
This is intended for checking a column of dates before coercing it.

## Usage

``` r
md_problems(x)
```

## Arguments

- x:

  A vector of dates, usually character.

## Value

A data frame with one row per problematic element and the columns
`index` (its position in `x`), `input` (the offending value), and
`reason`. A zero-row data frame means every element can be coerced.

## Examples

``` r
md_problems(c("2019-01-01", "2019-02-30", "2019-W12", "not a date"))
#>   index      input                                                     reason
#> 1     2 2019-02-30 '2019-02-30' has day '30', but that month has only 28 days
#> 2     4 not a date                              could not be parsed as a date
# a clean vector returns no rows
md_problems(c("2019-01-01", "2019-01"))
#> [1] index  input  reason
#> <0 rows> (or 0-length row.names)
```

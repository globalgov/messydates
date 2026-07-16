# Arithmetic operations for messydates

These operations allow users to add or subtract dates messydate objects.
Messydate objects include incomplete or uncertain dates, ranges of
dates, negative dates, and date sets.

## Usage

``` r
# S3 method for class 'mdate'
e1 + e2

# S3 method for class 'mdate'
e1 - e2
```

## Arguments

- e1:

  An `mdate` or date object.

- e2:

  An `mdate`, date, or numeric object. Must be a scalar.

## Value

A messydates vector

## Details

When `e2` is a number or a string naming a unit ("day", "week", "month",
"year", or, for date-times, "hour", "min"/"minute", or "sec"/"second"),
`e1` is shifted by that amount:

- A bare number, or a "day"/"week" unit, shifts by that many days.

- "month" and "year" shift the calendar component itself (preserving the
  day of month and any time of day), rolling back to the last valid day
  where necessary, e.g. adding a month to `"2012-01-31"` gives
  `"2012-02-29"` (2012 being a leap year), not an invalid
  `"2012-02-31"`.

- Sub-day units, or any unit at all when `e1` carries a time of day,
  shift the instant in seconds via `POSIXct`, promoting a date-only `e1`
  to a time if the shift is sub-day.

When `e2` is itself an `mdate`, `+`/`-` instead treat both sides as sets
of dates: `+` returns their union (as a multiset, in the most succinct
`mdate` notation; see also
[`?operate_set`](https://globalgov.github.io/messydates/reference/operate_set.md)
for `%union%`, which returns a plain vector of the member dates
instead), and `-` removes the dates in `e2` from `e1`.

## Examples

``` r
# \donttest{
d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "~2001",
"2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
"2008-XX-31", "..2002-02-03", "2001-01-03..", "28 BC"))
data.frame(date = d, add = d + 1, subtract = d - 1)
#>                       date                     add                subtract
#> 1               2008-03-25              2008-03-26              2008-03-24
#> 2              -2012-02-27             -2012-02-26             -2012-02-28
#> 3                 2001-01?  2001-01-02..2001-02-01  2000-12-31..2001-01-30
#> 4                    ~2001  2001-01-02..2002-01-01  2000-12-31..2001-12-30
#> 5   2001-01-01..2001-02-02  2001-01-02..2001-02-03  2000-12-31..2001-02-01
#> 6  {2001-01-01,2001-02-02} {2001-01-02,2001-02-03} {2000-12-31,2001-02-01}
#> 7               2008-XX-31              2008-02-01              2008-XX-30
#> 8             ..2002-02-03            ..2002-02-04            ..2002-02-02
#> 9             2001-01-03..            2001-01-04..            2001-01-02..
#> 10                   -0028  -0027-12-31..-28-12-30  -0028-01-02..-29-01-01
data.frame(date = d, add = d + "1 year", subtract = d - "1 year")
#>                       date                     add                subtract
#> 1               2008-03-25              2009-03-25              2007-03-26
#> 2              -2012-02-27             -2011-02-27             -2013-02-26
#> 3                 2001-01?                 2002-01  2000-01-02..2000-02-01
#> 4                    ~2001                    2002  2000-01-02..2000-12-31
#> 5   2001-01-01..2001-02-02  2002-01-01..2002-02-02  2000-01-02..2000-02-03
#> 6  {2001-01-01,2001-02-02} {2002-01-01,2002-02-02} {2000-01-02,2000-02-03}
#> 7               2008-XX-31              2009-XX-30              2007-XX-31
#> 8             ..2002-02-03            ..2003-02-03            ..2001-02-03
#> 9             2001-01-03..            2002-01-03..            2000-01-04..
#> 10                   -0028  -0027-01-01..-28-01-01  -0028-12-31..-29-12-31
as_messydate("2001-01-01") + as_messydate("2001-01-02..2001-01-04")
#>  'mdate' chr "2001-01-01..2001-01-04"
as_messydate("2001-01-01") + as_messydate("2001-01-03")
#>  'mdate' chr "{2001-01-01,2001-01-03}"
as_messydate("2001-01-01..2001-01-04") - as_messydate("2001-01-02")
#> [[1]]
#>  'mdate' chr "{2001-01-01,2001-01-03..2001-01-04}"
#> 
#as_messydate("2001-01-01") - as_messydate("2001-01-03")
# calendar (month/year) arithmetic keeps the day of month and time of day
as_messydate("2012-01-31 09:00") + "1 month"
#>  'mdate' chr "2012-02-29 09:00"
# sub-day units shift the instant
as_messydate("2012-01-01 14:30:00") + "2 hours"
#>  'mdate' chr "2012-01-01 16:30:00"
# }
```

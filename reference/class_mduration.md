# A flexible duration class for messy durations

Most R packages handle duration and periods as exact time or date
intervals. However, this is not possible for 'messy' dates where
uncertainty or approximation might be present. The `mduration` class
accounts for uncertainty and approximation in `mdate` objects to return
their duration as a range of possible dates.

Non-range values (a single date, or a range collapsed to a single value)
are returned unchanged. When both ends of the range carry a time of day,
`approx_range` is still interpreted as a number of days, but the
returned range keeps sub-day precision (e.g.
`"2010-01-01 09:00..2010-01-01 17:00"`).

## Usage

``` r
new_messyduration(x = character())

validate_messyduration(x, approx_range = 0)

make_messyduration(x, approx_range = 0)

# S3 method for class 'character'
make_messyduration(x, approx_range = 0)

# S3 method for class 'mdate'
make_messyduration(x, approx_range = 0)

# S3 method for class 'mduration'
print(x, ...)
```

## Arguments

- x:

  An `mdate` variable with ranges.

- approx_range:

  Range to expand approximate dates, in days. If 3, for example, widens
  the range by 3 days on both sides, moving the start 3 days earlier and
  the end 3 days later; if -3, narrows the range by 3 days from both
  sides.

- ...:

  Additional arguments passed to
  [`str()`](https://rdrr.io/r/utils/str.html).

## Value

Object of class `mduration`

## Examples

``` r
make_messyduration(as_messydate(c("2010-01-01..2010-12-31", "2010-01..2010-12")))
#>  'mduration' chr [1:2] "2010-01-01..2010-12-31" "2010-01-01..2010-12-31"
# widen (or narrow) the range at both ends
make_messyduration(as_messydate("2010-06-01..2010-06-10"), approx_range = 3)
#>  'mduration' chr "2010-05-29..2010-06-13"
# ranges that carry a time of day keep sub-day precision
make_messyduration(as_messydate("2010-01-01 09:00..2010-01-01 17:00"))
#>  'mduration' chr "2010-01-01 09:00:00..2010-01-01 17:00:00"
```

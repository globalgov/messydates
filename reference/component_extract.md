# Extracting components from messy dates

These functions allow the extraction of particular date components from
messy dates, such as the
[`year()`](https://lubridate.tidyverse.org/reference/year.html),
[`month()`](https://lubridate.tidyverse.org/reference/month.html),
[`day()`](https://lubridate.tidyverse.org/reference/day.html), and, for
date-times,
[`hour()`](https://lubridate.tidyverse.org/reference/hour.html),
[`minute()`](https://lubridate.tidyverse.org/reference/minute.html),
[`second()`](https://lubridate.tidyverse.org/reference/second.html), and
the time zone
([`tz()`](https://lubridate.tidyverse.org/reference/tz.html)).

These are methods for the same-named generics in `{lubridate}`, so they
extend rather than mask them: calling e.g.
[`year()`](https://lubridate.tidyverse.org/reference/year.html) on an
`mdate` returns the messy-date-aware result (understanding partial
precision such as `2012-06-XX`), while calling it on a `Date` or
`POSIXct` still dispatches to `{lubridate}`'s own methods. This lets
`{messydates}` and `{lubridate}` be loaded together, in either order,
without one masking the other.

`precision()` allows for the identification of the greatest level of
precision in (currently) the first element of each date.

## Usage

``` r
# S3 method for class 'mdate'
year(x, ...)

# S3 method for class 'mdate'
month(x, ...)

# S3 method for class 'mdate'
mday(x, ...)

# S3 method for class 'mdate'
hour(x, ...)

# S3 method for class 'mdate'
minute(x, ...)

# S3 method for class 'mdate'
second(x, ...)

# S3 method for class 'mdate'
tz(x, ...)

precision(x)

# S3 method for class 'mdate'
precision(x)
```

## Arguments

- x:

  A `mdate` object

- ...:

  Additional arguments passed to or from other methods (accepted for
  compatibility with the `{lubridate}` generics; unused).

## Value

[`year()`](https://lubridate.tidyverse.org/reference/year.html),
[`month()`](https://lubridate.tidyverse.org/reference/month.html),
[`day()`](https://lubridate.tidyverse.org/reference/day.html),
[`hour()`](https://lubridate.tidyverse.org/reference/hour.html),
[`minute()`](https://lubridate.tidyverse.org/reference/minute.html), and
[`second()`](https://lubridate.tidyverse.org/reference/second.html)
extraction return the integer for the requested component (`NA` where
the component is absent or unspecified).

[`tz()`](https://lubridate.tidyverse.org/reference/tz.html) returns the
time zone designator or offset as a string.

`precision()` returns the level of greatest precision for each date.

## Details

Unlike `{lubridate}`'s
[`tz()`](https://lubridate.tidyverse.org/reference/tz.html), which
returns an Olson time zone name, `tz.mdate()` returns the ISO 8601 UTC
offset *designator* carried by the date-time string (`"Z"` or e.g.
`"+02:00"`), or `NA` when none is present.

## Precision

Date precision is measured relative to the day in \\1/days(x)\\. That
is, a date measured to the day will return a precision score of 1, a
date measured to the month will return a precision score of between
\\1/28\\ and \\1/31\\, and annual measures will have a precision of
between \\1/365\\ and \\1/366\\. Times of day extend the same scale
below the day: a date-time measured to the hour returns 24, to the
minute 1440, and to the second 86400.

## Examples

``` r
year(as_messydate(c("2012-02-03","2012","2012-02")))
#> [1] 2012 2012 2012
month(as_messydate(c("2012-02-03","2012","2012-02")))
#> [1]  2 NA  2
day(as_messydate(c("2012-02-03","2012","2012-02")))
#> [1]  3 NA NA
hour(as_messydate(c("2012-02-03 14:30:00","2012-02-03")))
#> [1] 14 NA
minute(as_messydate("2012-02-03 14:30:00"))
#> [1] 30
second(as_messydate("2012-02-03 14:30:05"))
#> [1] 5
tz(as_messydate("2012-02-03 14:30:00+02:00"))
#> [1] "+02:00"
precision(as_messydate(c("2012-02-03","2012","2012-02")))
#> [1] 1.00000000 0.00273224 0.03448276
precision(as_messydate("2012-02-03 14:30"))
#> [1] 1440
```

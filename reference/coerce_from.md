# Coercion from `mdate` to common date classes

These functions coerce objects of `mdate` class to common date classes
such as `Date`, `POSIXct`, and `POSIXlt`. Since `mdate` objects can hold
multiple individual dates, however, an additional function must be
passed as an argument so that these functions know how to resolve
multiple dates into a single date.

For example, one might wish to use the earliest possible date in any
ranges of dates (`min`), the latest possible date (`max`), some notion
of a central tendency (`mean`, `median`, or `modal`), or even a `random`
selection from among the candidate dates.

These functions then, building on
[`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
and the resolve functions, are particularly useful in converting back
out of the `mdate` class for use with existing methods and models,
especially for checking the robustness of results.

## Usage

``` r
# S3 method for class 'mdate'
as.Date(x, FUN = vmin, ...)

# S3 method for class 'mdate'
as.POSIXct(x, tz = "UTC", FUN = vmin, ...)

# S3 method for class 'mdate'
as.POSIXlt(x, tz = "UTC", FUN = vmin, ...)

# S3 method for class 'mdate'
as.data.frame(x, ...)

# S3 method for class 'mdate'
as.list(x, ...)

# S3 method for class 'mdate'
as.double(x, ...)

# S4 method for class 'mdate'
as_datetime(x, ...)
```

## Arguments

- x:

  A `mdate` object

- FUN:

  A function that can be used to resolve expanded messy dates into a
  single date. For example,
  [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html),
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`median()`](https://rdrr.io/r/stats/median.html),
  [`modal()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  and
  [`random()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md).
  [`vmin()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md),
  [`vmax()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md),
  [`vmean()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  [`vmedian()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  [`vmodal()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  and
  [`vrandom()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  are the vectorised equivalents, resolving each element separately
  rather than summarising the whole vector.

- ...:

  Arguments passed on to the S3 generics.

- tz:

  Character string specifying the time zone for the conversion, if
  required. By default "UTC" (Universal Time Coordinated), equivalent to
  GMT. If "" then the current time zone is used.

## Value

A date object of `Date`, `POSIXct`, or `POSIXlt` class

## Details

[`as.Date()`](https://rdrr.io/r/base/as.Date.html) always drops any time
of day carried by `x` (a calendar date has no time component); use
[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) or
[`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html) to keep the
time.

[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) and
[`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html) keep the time
of day (defaulting to midnight if `x` is date-only), and honour a UTC
offset if `x` carries one. They do not support dates before the common
era; use [`as.Date()`](https://rdrr.io/r/base/as.Date.html) for those.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) places
the (unresolved) `mdate` vector in a single-column data frame, as for
any other vector.

[`as.list()`](https://rdrr.io/r/base/list.html) splits `x` into a list
of length-one `mdate` objects, one per element, without resolving any of
them.

[`as.double()`](https://rdrr.io/r/base/double.html) converts `x` to the
number of days since 1970-01-01 (as for `as.double(as.Date(x))`),
without resolving ranges, sets, or unspecified components first; it is
mostly useful for already-precise dates.

`{lubridate}`'s `as_date()` and `as_datetime()` also accept an `mdate`
(delegating to
[`as.Date()`](https://rdrr.io/r/base/as.Date.html)/[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html)
above, so the `FUN` resolver still applies).

## See also

[`resolve_extrema()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md),
[`resolve_tendency()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)

Other coerce:
[`coerce_to`](https://globalgov.github.io/messydates/reference/coerce_to.md)

## Examples

``` r
as.Date(as_messydate("2012-01"), FUN = vmin)
#> [1] "2012-01-01"
as.Date(as_messydate("2012-01-01"), FUN = vmean)
#> [1] "2012-01-01"
as.Date(as_messydate("2012-01"), FUN = vmax)
#> [1] "2012-01-31"
as.Date(as_messydate("2012-01"), FUN = vmedian)
#> [1] "2012-01-16"
as.Date(as_messydate("2012-01"), FUN = vmodal)
#> [1] "2012-01-01"
as.Date(as_messydate("2012-01"), FUN = vrandom)
#> [1] "2012-01-13"
# "1000 BC" is the astronomical year -0999 (year zero exists), whereas the
# signed ISO "-1000" below is astronomical year -1000
as.Date(as_messydate("1000 BC"), FUN = vmax)
#> [1] "-999-12-31"
as.Date(as_messydate("1000 BC"), FUN = vmedian)
#> [1] "-999-07-02"
as.Date(as_messydate(c("-1000", "2020")), FUN = vmin)
#> [1] "-1000-01-01" "2020-01-01" 
# the time of day, if any, is dropped
as.Date(as_messydate("2012-01-01 14:30"), FUN = vmin)
#> [1] "2012-01-01"
as.POSIXct(as_messydate("2012-01-01 14:30:00"), FUN = vmin)
#> [1] "2012-01-01 14:30:00 UTC"
as.POSIXct(as_messydate("2012-01-01 14:30:00+02:00"), FUN = vmin)
#> [1] "2012-01-01 12:30:00 UTC"
as.POSIXlt(as_messydate("2012-01-01 14:30:00"), FUN = vmin)
#> [1] "2012-01-01 14:30:00 UTC"
as.data.frame(as_messydate(c("2012-01-01", "2012-02")))
#>            x
#> 1 2012-01-01
#> 2    2012-02
as.list(as_messydate(c("2012-01-01", "2012-02")))
#> [[1]]
#>  'mdate' chr "2012-01-01"
#> 
#> [[2]]
#>  'mdate' chr "2012-02"
#> 
as.double(as_messydate("2012-01-01"))
#> [1] 15340
```

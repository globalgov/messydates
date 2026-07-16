# Sequence method for messydates

This function provides a sequence
([`seq()`](https://rdrr.io/r/base/seq.html)) method for messydates. This
can be used with ranges or unspecified dates, and is particularly useful
for defining a sequence of dates before the common era or between eras.

## Usage

``` r
# S3 method for class 'mdate'
seq(from, to, by = "days", ...)
```

## Arguments

- from:

  A messydate or range. If 'from' is a range and 'to' is not specified,
  'from' will be the minimum of the range and 'to' will be maximum.

- to:

  A messydate.

- by:

  Increment of the sequence. By default "days". Use a sub-day unit
  ("hour", "min", or "sec") for a date-time sequence.

- ...:

  Arguments passed to or from methods.

## Details

If `from`/`to` (or `by`) carry a time of day, the sequence is generated
at the requested sub-day granularity (e.g. `by = "hour"`) via `POSIXct`,
and each element of the result keeps a time of day. Otherwise, dates are
sequenced by calendar day (or another day-based `by`, e.g. `"week"` or
`"month"`), including across the boundary between BCE and CE dates.

## Examples

``` r
seq(mdate("-0001-12-20"), mdate("0001-01-10"))
#>  [1] "-0001-12-20" "-0001-12-21" "-0001-12-22" "-0001-12-23" "-0001-12-24"
#>  [6] "-0001-12-25" "-0001-12-26" "-0001-12-27" "-0001-12-28" "-0001-12-29"
#> [11] "-0001-12-30" "-0001-12-31" "0001-01-01"  "0001-01-02"  "0001-01-03" 
#> [16] "0001-01-04"  "0001-01-05"  "0001-01-06"  "0001-01-07"  "0001-01-08" 
#> [21] "0001-01-09"  "0001-01-10" 
# a range's endpoints are used when only 'from' is given
seq(as_messydate("2012-01-01..2012-01-05"))
#> [1] "2012-01-01" "2012-01-02" "2012-01-03" "2012-01-04" "2012-01-05"
# date-time sequences use a sub-day 'by'
seq(as_messydate("2019-03-01 09:00"), as_messydate("2019-03-01 12:00"),
    by = "hour")
#> [1] "2019-03-01 09:00:00" "2019-03-01 10:00:00" "2019-03-01 11:00:00"
#> [4] "2019-03-01 12:00:00"
```

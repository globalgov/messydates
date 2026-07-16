# Resolves messy dates into an extrema

This collection of S3 methods 'resolve' messy dates into a single date
according to some explicit bias, such as returning the minimum or
maximum date, the mean, median, or modal date, or a random date from
among the possible resolutions for each messy date. If the date is not
'messy' (i.e. has no annotations) then just that precise date is
returned. This can be useful for various descriptive or inferential
projects.

## Usage

``` r
vmin(..., na.rm = FALSE)

# S3 method for class 'mdate'
vmin(..., na.rm = FALSE)

# S3 method for class 'mdate'
min(..., na.rm = FALSE)

vmax(..., na.rm = FALSE)

# S3 method for class 'mdate'
vmax(..., na.rm = FALSE)

# S3 method for class 'mdate'
max(..., na.rm = FALSE)
```

## Arguments

- ...:

  a mdate object

- na.rm:

  Should NAs be removed? FALSE by default.

## Value

A single scalar or vector of dates

## Details

`vmin()`/[`min()`](https://rdrr.io/r/base/Extremes.html) and
`vmax()`/[`max()`](https://rdrr.io/r/base/Extremes.html) work directly
on the annotated string (dropping `~`/`?`/`%` and, for
`vmax()`/[`max()`](https://rdrr.io/r/base/Extremes.html), filling in
unspecified `X` components) rather than via
[`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md),
which makes them considerably faster than resolving through the full
expanded set. [`min()`](https://rdrr.io/r/base/Extremes.html) and
[`max()`](https://rdrr.io/r/base/Extremes.html) further resolve a vector
to a single, overall extremum (matching the usual behaviour of these
generics), while `vmin()` and `vmax()` resolve each element separately.

Dates that carry a time of day are already precise and so pass through
these functions unchanged, keeping their time; a time of day plays no
further role in choosing the minimum or maximum.

## Examples

``` r
d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
  "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
  "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
d
#>  'mdate' chr [1:9] "2008-03-25" "?2012-02-27" "2001-01?" "~2001" ...
# a precise date-time is returned unchanged
vmin(as_messydate("2012-01-01 14:30:00"))
#>  'mdate' chr "2012-01-01 14:30:00"
vmin(d)
#>  'mdate' chr [1:9] "2008-03-25" "2012-02-27" "2001-01-01" "2001-01-01" ...
min(d)
#>  'mdate' chr "-0050-01-01"
vmax(d)
#>  'mdate' chr [1:9] "2008-03-25" "2012-02-27" "2001-01-31" "2001-12-31" ...
max(d)
#>  'mdate' chr "2012-02-27"
```

# Resolves messy dates into a central tendency

These functions resolve messydates by their central tendency. While the
functions [`mean()`](https://rdrr.io/r/base/mean.html),
[`median()`](https://rdrr.io/r/stats/median.html), and `modal()` expand
*all* elements of the vector into one combined set of dates and
summarise it to a single value (matching the usual behaviour of these
generics), the `v*()` versions resolve each element separately and so
return a vector of the same length as the input.

## Usage

``` r
# S3 method for class 'mdate'
median(..., na.rm = TRUE)

vmedian(..., na.rm = TRUE)

# S3 method for class 'mdate'
vmedian(..., na.rm = TRUE)

# S3 method for class 'mdate'
mean(..., trim = 0, na.rm = TRUE)

vmean(..., na.rm = TRUE)

# S3 method for class 'mdate'
vmean(..., trim = 0, na.rm = TRUE)

modal(..., na.rm = TRUE)

# S3 method for class 'mdate'
modal(..., na.rm = TRUE)

vmodal(..., na.rm = TRUE)

# S3 method for class 'mdate'
vmodal(..., na.rm = TRUE)

random(..., na.rm = TRUE)

# S3 method for class 'mdate'
random(..., na.rm = TRUE)

vrandom(..., na.rm = TRUE)

# S3 method for class 'mdate'
vrandom(..., na.rm = TRUE)
```

## Arguments

- ...:

  a mdate object

- na.rm:

  Should NAs be removed? FALSE by default.

- trim:

  the fraction (0 to 0.5) of observations to be trimmed from each end of
  x before the mean is computed. Values of trim outside that range are
  taken as the nearest endpoint.

## Details

All of these functions work by calling
[`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
to enumerate the dates or date-times consistent with each messy value,
then summarising that expanded set. For
[`median()`](https://rdrr.io/r/stats/median.html) and
[`mean()`](https://rdrr.io/r/base/mean.html), an even number of expanded
values is resolved by averaging the two middle values (via `POSIXct`
when a time of day is present, or `Date` otherwise); an odd number
simply returns the middle value.

Both kinds of set are expanded to their members and summarised the same
way, but the result means different things for each. For a
[`{}`](https://rdrr.io/r/base/Paren.html) ("all members of") set, which
records that something happened on several dates, the central tendency
describes where those occurrences sit. For a `[]` ("one member of") set,
which records that something happened on exactly one of those dates but
not which, the central tendency is a point estimate of that single
unknown date, as it is for a range or an unspecified component.
`vrandom()` draws a member uniformly in either case, which for a `[]`
set is a draw from the candidates themselves.

Averaging across the BCE/CE boundary, or between two BCE dates, is not
currently supported: [`median()`](https://rdrr.io/r/stats/median.html)
falls back to the earlier of the two middle values in that case, and
[`mean()`](https://rdrr.io/r/base/mean.html)/`vmean()` may be unreliable
for solely negative-year inputs (a documented limitation, not a
supported feature).

## Examples

``` r
d <- as_messydate(c("2008-03-25", "?2012-02-27", "2001-01?", "2001~",
  "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}",
  "{2001-01,2001-02-02}", "2008-XX-31", "-0050-01-01"))
d
#>  'mdate' chr [1:9] "2008-03-25" "?2012-02-27" "2001-01?" "~2001" ...
# the time of day is honoured when averaging precise date-times
r <- as_messydate(c("2012-06-01 09:00", "2012-06-01 17:00"))
median(r)
#> [1] "2012-06-01 13:00:00"
mean(r)
#> [1] "2012-06-01 13:00:00"
median(d)
#> [1] "2001-05-20"
vmedian(d)
#> [1] "2008-03-25"  "2012-02-27"  "2001-01-16"  "2001-07-02"  "2001-01-17" 
#> [6] "2001-01-17"  "2001-01-16"  "2008-07-15"  "-0050-01-01"
mean(d)
#> [1] "1997-07-15"
vmean(d)
#> [1] "2008-03-25" "2012-02-27" "2001-01-16" "2001-07-02" "2001-01-17"
#> [6] "2001-01-17" "2001-01-16" "2008-07-15" "50-01-01"  
modal(d)
#> [1] "2001-01-01"
vmodal(d)
#> [1] "2008-03-25"  "2012-02-27"  "2001-01-01"  "2001-01-01"  "2001-01-01" 
#> [6] "2001-01-01"  "2001-01-01"  "2008-01-31"  "-0050-01-01"
random(d)
#> [1] "2001-01-09"
vrandom(d)
#> [1] "2008-03-25"  "2012-02-27"  "2001-01-05"  "2001-12-24"  "2001-01-31" 
#> [6] "2001-02-02"  "2001-01-06"  "2008-09-30"  "-0050-01-01"
```

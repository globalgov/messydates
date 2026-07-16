# A flexible date class for messy dates

Recent extensions to standardised date notation in [ISO
8601-2_2019(E)](https://www.iso.org/standard/70908.html) enable the
recording of dates as unspecified, uncertain, approximate, and as a set
or a range. These functions create and validate a new date class for R
that can contain and parse these annotations. Whereas `new_messydate()`
creates a new `mdate` object, and `make_messydate()` creates a new
`mdate` object from one, two, or three date variables,
`validate_messydate()` checks that the object is valid.

Note that the functions documented here are typically used internally,
not by users; users are recommended to use
[`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
to coerce dates and character strings, including historical prose, into
`mdate` objects.

## Usage

``` r
new_messydate(x = character())

validate_messydate(x)

make_messydate(..., resequence = FALSE)

# S3 method for class 'mdate'
print(x, ...)

# S3 method for class 'mdate'
format(x, ...)
```

## Arguments

- x:

  A character scalar or vector in the expected `"yyyy-mm-dd"` format
  annotated, as necessary, according to ISO 8601-2_2019(E).

- ...:

  One (yyyy-mm-dd), two (yyyy-mm-dd, yyyy-mm-dd), or three (yyyy, mm,
  dd) variables.

- resequence:

  Users have the option to choose the order for ambiguous dates with or
  without separators (e.g. "11-01-12" or "20112112"). `NULL` by default.
  Other options include: 'dmy', 'ymd', 'mdy', 'ym', 'my' and
  'interactive' If 'dmy', dates are converted from DDMMYY format for 6
  digit dates, or DDMMYYYY format for 8 digit dates. If 'ymd', dates are
  converted from YYMMDD format for 6 digit dates, or YYYYMMDD format for
  8 digit dates. If 'mdy', dates are converted from MMDDYY format for 6
  digit dates or MMDDYYYY format for 8 digit dates. For these three
  options, ambiguous dates are converted to YY-MM-DD format for 6 digit
  dates, or YYYY-MM-DD format for 8 digit dates. If 'my', ambiguous 6
  digit dates are converted from MM-YYYY format to YYYY-MM. If 'ym',
  ambiguous 6 digit dates are converted to YYYY-MM format. If
  'interactive', it prompts users to select the existing component order
  of ambiguous dates, based on which the date is reordered into
  YYYY-MM-DD format and further completed to YYYY-MM-DD format if they
  choose to do so.

## Value

Object of class `mdate`

## Details

If three date variables are passed to `make_messydate()`, function will
create a single date (yyyy-mm-dd) from it. If two date variables are
passed to `make_messydate()`, function will create a range of dates from
it (yyyy-mm-dd..yyyy-mm-dd). If one date variable is passed to
`make_messydate()`, function defaults to
[`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md).

[`format()`](https://rdrr.io/r/base/format.html) returns the underlying
ISO 8601-2 strings, so that `mdate` vectors render legibly when held in
a `data.frame` or tibble column (which format their columns rather than
printing them).

## Dates and times

Since v1.0.0, messydates operates with and on both dates and times.
Times of day may be appended to a date using a space, e.g.
`2019-03-01 14:30:00`. ISO 8601-1 sec. 4.3.2 and RFC 3339 both permit a
space as an alternative to the `T` separator more commonly seen in
machine-generated timestamps; messydates uses a space for readability,
though `T` continues to be accepted on input. Hours, minutes, and
seconds are accepted (with optional fractional seconds), as are 12-hour
`am`/`pm` times. Coordinated Universal Time is written with the `Z`
designator, and other zones as a numeric offset, e.g. `+02:00`. Time
components accept the same annotations as dates: approximate (`~`),
uncertain (`?`), both (`%`), and unspecified (`X`), e.g.
`2019-03-01 ~14:30` or `2019-03-01 14:XX`. Because `:` is also used as a
range separator, times are detected and protected before ranges are
parsed, so `2009-01-01:2019-01-01` remains a range while
`2019-03-01 14:30:00` is read as a time. A time of day may also be given
on its own, with no date part, e.g. `14:30` or `2:30pm`. This requires a
clear time signal (a colon-separated clock or an `am`/`pm` suffix), so a
bare number such as `2019` is still read as a year rather than an hour;
a bare `am`/`pm` hour (`2pm`) is taken as an exact hour and filled to
`14:00`.

## Imprecision annotations

*Unspecified date components*, such as when the day is unknown, can be
represented by one or more `X`s in place of the digits. The modifier `*`
is recommended to indicate that the entire time scale component value is
unspecified, e.g. `X*-03-03`, however this is not implemented here.
Please be explicit about the digits that are unspecified, e.g.
`XXXX-03-03` expresses 3rd March in some unspecified year, whereas
`2003-XX-03` expresses the 3rd of some month in 2003. If time components
are not given, they are expanded to this.

*Approximate date components*, modified by `~`, represent an estimate
whose value is asserted to be possibly correct. For example,
`2003~-03-03` The degree of confidence in approximation depends on the
application.

*Uncertain date components*, modified by `?`, represent a date component
whose source is considered to be dubious and therefore not to be relied
upon. An additional modifier, `%`, is used to indicate a value that is
both uncertain and approximate.

## Set annotations

These functions also introduce standard notation for ranges of dates.
Rather than the typical R notation for ranges, `:`, ISO 8601-2_2019(E)
recommends `..`. This then can be applied between two time scale
components to create a standard range between these dates (inclusive),
e.g. `2009-01-01..2019-01-01`. But it can also be used as an affix,
indicating "on or before" if used as a prefix, e.g. `..2019-01-01`, or
indicating "on or after" if used as a suffix, e.g. `2009-01-01..`.

And lastly, notation for sets of dates is also included. Here braces,
[`{}`](https://rdrr.io/r/base/Paren.html), are used to mean "all members
of the set", while brackets, `[]`, are used to mean "one member of the
set".

## See also

[`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
for the full, user-facing coercion pipeline, including parsing of free
text and historical prose (e.g. Roman numerals, "circa", "between ...
and ...").

## Examples

``` r
new_messydate("2012-03-03")
#>  'mdate' chr "2012-03-03"
validate_messydate(new_messydate(c("2012-03-03", "2012-XX-03~")))
#>  'mdate' chr [1:2] "2012-03-03" "2012-XX-03~"
# invalid characters or missing digits raise an error
tryCatch(validate_messydate(new_messydate("2012-03-03g")),
         error = function(e) e$message)
#> [1] "The only alpha characters allowed in messy dates are 'X' for\n      unspecified components, and 'T'/'Z' for times"
make_messydate("2010", "10", "10")
#>  'mdate' chr "2010-10-10"
```

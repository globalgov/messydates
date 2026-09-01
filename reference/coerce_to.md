# Coercion from common date classes to `mdate`

These methods coerce various date classes into the `mdate` class. They
represent the main user-facing class-creating functions in the package.
In addition to the typical date classes in R (`Date`, `POSIXct`, and
`POSIXlt`), there is also a direct method for converting text or
character strings to `mdate`. The function can also extract dates and
times from text, including some historical prose conventions, though
this is a work-in-progress and currently only works in English.

## Usage

``` r
as_messydate(x, resequence = FALSE)

# S3 method for class 'Date'
as_messydate(x, resequence = FALSE)

# S3 method for class 'POSIXct'
as_messydate(x, resequence = FALSE)

# S3 method for class 'POSIXlt'
as_messydate(x, resequence = FALSE)

# S3 method for class 'character'
as_messydate(x, resequence = NULL)

# S3 method for class 'factor'
as_messydate(x, resequence = NULL)

# S3 method for class 'numeric'
as_messydate(x, resequence = NULL)

# S3 method for class 'list'
as_messydate(x, resequence = FALSE)

mdate(x, resequence = FALSE)
```

## Arguments

- x:

  A scalar or vector of a class that can be coerced into `mdate`, such
  as `Date`, `POSIXct`, `POSIXlt`, or character.

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

A `mdate` class object

## Details

Coercion from `POSIXct` and `POSIXlt` preserves the time of day (and UTC
offset) as an ISO 8601-2 date-time. Times of exactly midnight
(`00:00:00`) are treated as date-only, so that timezone-naive dates
round-trip unchanged.

## Functions

- `as_messydate()`: Core `mdate` class coercion function

- `as_messydate(Date)`: Coerce from `Date` to `mdate` class

- `as_messydate(POSIXct)`: Coerce from `POSIXct` to `mdate` class

- `as_messydate(POSIXlt)`: Coerce from `POSIXlt` to `mdate` class

- `as_messydate(character)`: Coerce character date objects to `mdate`
  class

- `as_messydate(factor)`: Coerce factors to `mdate` class, via their
  labels. This is the common case when a date column has been read in
  with `stringsAsFactors = TRUE`.

- `as_messydate(numeric)`: Coerce numeric objects to `mdate` class

- `as_messydate(list)`: Coerce list date objects to the most concise
  representation of `mdate` class

## Parsing prose

Beyond plain and lightly-formatted dates, `as_messydate()` recognises
several conventions common in e.g. historical texts and converts them to
their ISO 8601-2 equivalent before the usual parsing takes place:

- Roman numerals for a bare year, e.g. `"MDCCLXXVI"` becomes `1776`.

- Roman calendar references, i.e. the Kalends, Nones, and Ides of a
  named month, e.g. `"the Ides of March, 44 BC"` becomes
  `"-0043-03-15"`. The Nones and Ides fall later (the 7th and 15th) in
  March, May, July, and October, and earlier (the 5th and 13th) in other
  months.

- Approximate qualifiers ("around", "circa", "about", "roughly", ...)
  add the `~` annotation, and uncertain qualifiers ("possibly",
  "perhaps", "reportedly", ...) add `?`; both together add `%`, e.g.
  `"possibly about 1910"` becomes `"%1910"`.

- Connectives joining two days of the same month: "between the 13th and
  15th" or "from the 13th to the 15th" become a range (`..`); "the 13th
  or the 15th" becomes a set
  ([`{}`](https://rdrr.io/r/base/Paren.html)); and a plain "the 13th and
  the 15th" becomes two separate dates.

- "before"/"prior to"/"no later than" and "after"/"since"/"no earlier
  than" become an open range, e.g. `"before 1910"` becomes `"..1910"`.
  The bound may itself be any precision the parser understands,
  including a decade or century.

- Decades ("the 1920s" becomes `"192X"`) and centuries ("the 19th
  century" becomes `"18XX"`).

- A day of the year, e.g. `"the 103rd day of 2026"` becomes
  `"2026-04-13"`. The ISO 8601-2 notation for this (`"2026-103"`) is
  rejected instead of read, since it is easily read as a mistake for
  `"2026-10-03"`.

- A week of the year, e.g. `"the 5th week of 2026"` becomes the range
  `"2026-01-26..2026-02-01"`. Weeks follow the ISO 8601 rule: a week
  starts on a Monday, and week 1 is the week holding the first Thursday
  of the year, so week 1 can start in December of the previous year. The
  ISO 8601-2 week date is read too, as `"2026-W05"` or, for one day
  within the week, `"2026-W05-3"`, but it is never written back in that
  notation.

- Seasons, e.g. `"summer 2026"` becomes the month range
  `"2026-06..2026-08"`. These are the northern-hemisphere meteorological
  seasons, spring running from March to May and winter from December
  into February of the following year. The EDTF season codes
  (`"2026-22"`) are rejected instead of read, being easily read as a
  mistake for a date.

- A comma-separated list of dates in prose, e.g.
  `"13th Feb, 1977, Feb 15 1977, 1910"`, is split into separate dates
  (here, three): a fragment that is only a year is treated as completing
  the date before it.

## Eras and year numbering

`{messydates}` stores years in the ISO 8601-2 (proleptic Gregorian)
astronomical numbering, in which a year zero exists and equals 1 BCE,
`-0001` equals 2 BCE, and so on. Historical "BC"/"BCE" prose uses the
older convention that has no year zero, so it is converted on input: a
historical year `N BCE` becomes the astronomical year `-(N-1)`, e.g.
`"1 BCE"` becomes `"0000"`, `"2 BCE"` becomes `"-0001"`, and `"44 BCE"`
becomes `"-0043"`. A year written directly in signed ISO form (e.g.
`"-0044"`) is already astronomical and is left unchanged, so
`as_messydate("-0044")` (astronomical year -44, i.e. 45 BCE) and
`as_messydate("44 BCE")` (`"-0043"`) intentionally differ. "AD"/"CE"
prose is simply dropped, the year being unchanged.

Era prose is a matter of input only: it is always removed on parsing,
the era of a date in an `mdate` being carried by the sign of its year
alone. Where it appears in the input, it is resolved for each year
separately. A marker written before a date governs that date
(`"{BC1044-03-15,BC1033}"`); otherwise a year takes the era of the first
marker written after it, so a marker given once at the end still applies
to every bound of a range or set: `"200..100 BC"`, `"..200 BC"` and
`"200 BC.."` are all wholly BCE, whereas `"200 BC..100 AD"` spans the
two eras and gives `"-0199..0100"`.

## See also

Other coerce:
[`coerce_from`](https://globalgov.github.io/messydates/reference/coerce_from.md)

## Examples

``` r
as_messydate("2021")
#>  'mdate' chr "2021"
as_messydate("2021-02")
#>  'mdate' chr "2021-02"
as_messydate("2021-02-01")
#>  'mdate' chr "2021-02-01"
as_messydate("01-02-2021")
#>  'mdate' chr "2021-02-01"
as_messydate("1 February 2021")
#>  'mdate' chr "2021-02-01"
as_messydate("First of February, two thousand and twenty-one")
#>  'mdate' chr "2021-02-01"
as_messydate("2021-02-01?")
#>  'mdate' chr "2021-02-01?"
as_messydate("2021-02-01~")
#>  'mdate' chr "2021-02-01~"
as_messydate("2021-02-01%")
#>  'mdate' chr "2021-02-01%"
as_messydate("2021-02-01..2021-02-28")
#>  'mdate' chr "2021-02-01..2021-02-28"
as_messydate("{2021-02-01,2021-02-28}")
#>  'mdate' chr "{2021-02-01,2021-02-28}"
as_messydate(c("-2021", "2021 BC", "-2021-02-01"))
#>  'mdate' chr [1:3] "-2021" "-2020" "-2021-02-01"
as_messydate(c("210201", "20210201"), resequence = "ymd")
#>  'mdate' chr [1:2] "0021-02-01" "2021-02-01"
as_messydate(c("010221", "01022021"), resequence = "dmy")
#>  'mdate' chr [1:2] "0021-02-01" "2021-02-01"
# as_messydate(c("01-02-21", "01-02-2021", "01-02-91", "01-02-1991"),
# resequence = "interactive")
# ISO 8601-2 times, with the same annotations available on time components
as_messydate("2019-03-01 14:30:00Z")
#>  'mdate' chr "2019-03-01 14:30:00Z"
as_messydate("2019-03-01 2:30pm")
#>  'mdate' chr "2019-03-01 14:30"
as_messydate("2019-03-01 ~14:30")
#>  'mdate' chr "2019-03-01 ~14:30"
# a time of day may also be given on its own, with no date part
as_messydate("2:30pm")
#>  'mdate' chr "14:30"
as_messydate("around 2pm")
#>  'mdate' chr "14:00~"
# historical prose (see the "Parsing historical prose" section below)
as_messydate("MDCCLXXVI")
#>  'mdate' chr "1776"
as_messydate("the Ides of March, 44 BC")
#>  'mdate' chr "-0043-03-15"
as_messydate("possibly about 1910")
#>  'mdate' chr "%1910"
as_messydate("the 1920s")
#>  'mdate' chr "192X"
as_messydate("the 19th century")
#>  'mdate' chr "18XX"
as_messydate("the 103rd day of 2026")
#>  'mdate' chr "2026-04-13"
as_messydate("the 5th week of 2026")
#>  'mdate' chr "2026-01-26..2026-02-01"
as_messydate("2026-W05")
#>  'mdate' chr "2026-01-26..2026-02-01"
as_messydate("summer 2026")
#>  'mdate' chr "2026-06..2026-08"
as_messydate("before 1910")
#>  'mdate' chr "..1910"
as_messydate("between the 13th and 15th of Feb, 1977")
#>  'mdate' chr "1977-02-13..1977-02-15"
as_messydate(list(c("2012-06-01", "2012-06-02", "2012-06-03")))
#> [[1]]
#>  'mdate' chr "2012-06-01..2012-06-03"
#> 
as_messydate(list(c("2012-06-01", "2012-06-02", "2012-06-03",
"{2012-06-01, 2012-06-02, 2012-06-03}", "2012-06-01", "2012-06-03")))
#> [[1]]
#>  'mdate' chr "{2012-06-01..2012-06-03,2012-06-01..2012-06-03,2012-06-01,2012-06-03}"
#> 
```

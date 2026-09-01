# messydates ![messydates package logo](reference/figures/messydates_hexlogo.png)

## Why this package?

Existing packages for working with dates and times in R expect them to
be *tidy*. That is, they should be in or coercible to the standard
`yyyy-mm-dd` and/or `hh:mm:ss` format.

But dates (and times) are often ***messy***. Sometimes we only know the
year when something happened, leaving other components of the date, such
as the month or day, *unspecified*. This is often the case with
historical dates, for instance. Sometimes we can only say
*approximately* when an event occurred, that it occurred *before* or
*after* a certain date, or we recognise that our best estimate comes
from a *dubious* source. Other times there exists a *set* or *range* of
possible dates for an event.

Although researchers generally recognise this messiness, many feel
expected to force artificial precision or unfortunate imprecision on
temporal data to proceed with analysis. For example, if we only know
that, for one observation, it happened sometime in `2021`, then we might
revert to a panel data design *even if greater precision is available*
throughout the rest of the dataset, or opt to replace this date with the
start of that year (`2021-01-01`), assuming that erring on the earlier
(or later) side is more justifiable than a random date within that month
or year.

However, this can create inferential issues when timing or sequence is
important. [messydates](https://globalgov.github.io/messydates/) assists
with this problem by retaining, representing, and reasoning about messy
dates and times, rather than discarding them. It implements ISO
8601-2:2019 in R, introducing a new `mdate` class that can represent a
wide range of messy dates and times, and provides methods for coercing
into and from this class, as well as tools for working with messy dates
and times in a way that is compatible with existing packages.

### How does this compare to other date packages?

Base R and most date packages are built to convert input into a *single,
precise* instant, discarding anything that does not fit.
[messydates](https://globalgov.github.io/messydates/) instead treats
imprecision as information to be **retained, represented, and reasoned
about** — and only resolved to a precise date when you explicitly ask.

| Capability | base R | [lubridate](https://lubridate.tidyverse.org) | [anytime](https://github.com/eddelbuettel/anytime) | [clock](https://clock.r-lib.org) | [messydates](https://globalgov.github.io/messydates/) |
|----|:--:|:--:|:--:|:--:|:--:|
| Precise dates | ✓ | ✓ | ✓ | ✓ | ✓ |
| Precise times of day | ✓ | ✓ | ✓ | ✓ | ✓ |
| Parse flexible/ambiguous formats |  | ✓ | ✓ | ~ | ✓ |
| Historical & era dates (BCE, far future) |  | ~ |  | ~ | ✓ |
| Retain unspecified components (year- or month-only) |  |  |  |  | ✓ |
| Approximate (`~`) & uncertain (`?`) annotations |  |  |  |  | ✓ |
| Ranges, sets, and on-or-before/after dates |  |  |  |  | ✓ |
| Times with uncertainty/approximation |  |  |  |  | ✓ |
| Written/prose dates (e.g. “First of February…”) |  |  |  |  | ✓ |
| Days, weeks, and seasons of a year in prose |  |  |  |  | ✓ |
| Expand to (and contract from) all compatible dates |  |  |  |  | ✓ |
| Transparent resolution (min/max/mean/median/random) |  |  |  |  | ✓ |
| ISO 8601-2 (EDTF) support |  |  |  |  | ~ |

(✓ = supported, ~ = partially supported.
[clock](https://clock.r-lib.org) excels at high-precision arithmetic on
*valid, complete* dates, which complements rather than overlaps
[messydates](https://globalgov.github.io/messydates/).)

[messydates](https://globalgov.github.io/messydates/) covers the parts
of ISO 8601-2 that carry imprecision: unspecified components,
approximate and uncertain annotations, ranges, open ranges, sets, and
times of day. Durations are covered too, by the `mduration` class, which
expresses them as a range of possible dates so that uncertainty at
either end is preserved. Week dates (`2019-W12`) are read and converted
to the range of days they name. Not implemented are significant digits
(`1234S3`), extended years (`Y17E7`), the `P`-style duration notation
(`P1Y2M`), and repeating intervals; these are rejected on coercion
rather than silently passed through. Ordinal dates (`2019-123`) and
season codes (`2019-21`) are rejected too, being too easily read as a
mistake for `2019-12-03` or `2019-02-01`, but both are parsed when they
are written out: “the 123rd day of 2019” and “spring 2019”.

## A quick overview

[messydates](https://globalgov.github.io/messydates/) implements the
Extended Date/Time Format (EDTF) annotations set by the International
Organization for Standardization (ISO) outlined in [ISO
8601-2_2019(E)](https://www.iso.org/standard/70908.html) for R.
[messydates](https://globalgov.github.io/messydates/) introduces a new
`mdate` class that embeds these annotations, and offers a set of methods
for constructing and coercing into and from the `mdate` class, as well
as tools for working with such ‘messy’ dates.

``` r

pkg_comparison <- data.frame(
  Example = c("Normal date", "Future date", "Historical date", "Era date",
              "Written date", "DMY date", "MDY date", "Wrongly specified date",
              "Approximate date", "Uncertain date", "Unspecified date",
              "Censored date", "Range of dates", "Set of dates",
              "Date-time", "Approximate time"),
  OriginalDate = c("2012-01-01", "2599-12-31", "476", "33 BC",
                   "First of February, two thousand and twelve", "10-31-2012",
                   "31-10-2012", "2012-31-10", "2012-01-12~", "2012-01-01?",
                   "2012-01", "..2012-01-12", "2012-11-01:2012-12-01",
                   "2012-5-26, 2012-11-19, 2012-12-4",
                   "2012-01-01 14:30", "2012-01-01 ~14:30"),
  stringsAsFactors = FALSE)
pkg_comparison$base <- as.Date(pkg_comparison$OriginalDate)
pkg_comparison$lubridate <-
  suppressWarnings(lubridate::as_date(pkg_comparison$OriginalDate))
pkg_comparison$messydates <-
  messydates::as_messydate(pkg_comparison$OriginalDate)
```

| Example | OriginalDate | base | lubridate | messydates |
|:---|:---|:---|:---|:---|
| Normal date | 2012-01-01 | 2012-01-01 | 2012-01-01 | 2012-01-01 |
| Future date | 2599-12-31 | 2599-12-31 | 2599-12-31 | 2599-12-31 |
| Historical date | 476 | NA | NA | 0476 |
| Era date | 33 BC | NA | NA | -0032 |
| Written date | First of February, two thousand and twelve | NA | NA | 2012-02-01 |
| DMY date | 10-31-2012 | NA | NA | 2012-10-31 |
| MDY date | 31-10-2012 | 0031-10-20 | NA | 2012-10-31 |
| Wrongly specified date | 2012-31-10 | NA | NA | 2012-10-31 |
| Approximate date | 2012-01-12~ | 2012-01-12 | 2012-01-12 | 2012-01-12~ |
| Uncertain date | 2012-01-01? | 2012-01-01 | 2012-01-01 | 2012-01-01? |
| Unspecified date | 2012-01 | NA | 2020-12-01 | 2012-01 |
| Censored date | ..2012-01-12 | NA | 2012-01-12 | ..2012-01-12 |
| Range of dates | 2012-11-01:2012-12-01 | 2012-11-01 | 2012-11-01 | 2012-11-01..2012-12-01 |
| Set of dates | 2012-5-26, 2012-11-19, 2012-12-4 | 2012-05-26 | NA | {2012-05-26,2012-11-19,2012-12-04} |
| Date-time | 2012-01-01 14:30 | 2012-01-01 | 2020-12-01 | 2012-01-01 14:30 |
| Approximate time | 2012-01-01 ~14:30 | 2012-01-01 | 2020-12-01 | 2012-01-01 ~14:30 |

As can be seen in the table above, other date/time packages in R do not
handle ‘messy’ dates well. Normal “yyyy-mm-dd” structures or other date
formats that can easily be coerced into this structure are usually not a
problem.

However, some syntaxes are entirely ignored, such as historical dates
and dates from other eras (e.g. BCE), as well as written dates,
frequently used in historical texts or treaties.

Other times, existing packages return a date, but strip away any
annotations that express uncertainty or approximateness, introducing
artificial precision.

And sometimes returning only a single date means ignoring other
information included. We see this here in how only the end of the
censored date, only the start of the date range, or the first in the set
of dates is returned. Sometimes date components even seem guessed, such
as how `2021-01` (January 2021) is assumed to be 1 *December* 2021 by
[lubridate](https://lubridate.tidyverse.org).

So only [messydates](https://globalgov.github.io/messydates/) enables
researchers to retain all this information. But most analysis does still
expect some precision in dates to work.

## Working with messy dates

The first way that [messydates](https://globalgov.github.io/messydates/)
assists researchers that use dates in `mdate` class is to provide
methods for converting back into common date classes such as `Date`,
`POSIXct`, and `POSIXlt`. It is thus fully compatible with packages such
as [lubridate](https://lubridate.tidyverse.org) and `{anydate}`.

As messy date annotations can indicate multiple possible dates,
[messydates](https://globalgov.github.io/messydates/) allows e.g. ranges
or sets of dates to be unpacked or expanded into all compatible dates.

Since most methods of analysis or modelling expect single date
observations, we offer ways to resolve this multiplicity when coercing
`mdate`-class objects into other date formats. For example, researcher
might explicitly choose to favour the
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html),
[`mean()`](https://rdrr.io/r/base/mean.html),
[`median()`](https://rdrr.io/r/stats/median.html), or even a
[`random()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
date. This greatly facilitates research transparency by demanding a
conscious choice from researchers, as well as supporting robustness
checks by enabling description or inference across dates compatible with
the messy annotated date.

``` r

md <- pkg_comparison$messydates
resolve_mdate <- data.frame(
  messydates = as.character(md),
  min = md |> as.Date(vmin),
  median = md |> as.Date(vmedian),
  max = md |> as.Date(vmax),
  stringsAsFactors = FALSE)
```

| messydates                         | min        | median     | max        |
|:-----------------------------------|:-----------|:-----------|:-----------|
| 2012-01-01                         | 2012-01-01 | 2012-01-01 | 2012-01-01 |
| 2599-12-31                         | 2599-12-31 | 2599-12-31 | 2599-12-31 |
| 0476                               | 0476-01-01 | 0476-07-01 | 0476-12-31 |
| -0032                              | -032-01-01 | -032-07-02 | -032-12-31 |
| 2012-02-01                         | 2012-02-01 | 2012-02-01 | 2012-02-01 |
| 2012-10-31                         | 2012-10-31 | 2012-10-31 | 2012-10-31 |
| 2012-10-31                         | 2012-10-31 | 2012-10-31 | 2012-10-31 |
| 2012-10-31                         | 2012-10-31 | 2012-10-31 | 2012-10-31 |
| 2012-01-12~                        | 2012-01-12 | 2012-01-12 | 2012-01-12 |
| 2012-01-01?                        | 2012-01-01 | 2012-01-01 | 2012-01-01 |
| 2012-01                            | 2012-01-01 | 2012-01-16 | 2012-01-31 |
| ..2012-01-12                       | 2012-01-12 | 2012-01-12 | 2012-01-12 |
| 2012-11-01..2012-12-01             | 2012-11-01 | 2012-11-16 | 2012-12-01 |
| {2012-05-26,2012-11-19,2012-12-04} | 2012-05-26 | 2012-11-19 | 2012-12-04 |
| 2012-01-01 14:30                   | 2012-01-01 | 2012-01-01 | 2012-01-01 |
| 2012-01-01 ~14:30                  | 2012-01-01 | 2012-01-01 | 2012-01-01 |

As can be seen in the table above, all ‘precise’ dates are respected as
such, and returned no matter what ‘resolution’ function is given. But
for messy dates, the choice of function can make a difference. Where
only a year is given, e.g. `0476` or `-0032`, we draw from all the days
in the year. The minimum is the first of January and the maximum the
31st of December. Dates are also drawn from a set or range of dates when
given.

When only an approximate or censored date is known, then depending on
whether the whole date or just a component of the date is annotated,
then a range of dates is imputed based on some window (by default 3
years, months, or days), and then a precise date is resolved from that.

This translation via an expanded list of compatible dates is fast,
robust, and extensible, allowing researchers to use messy dates in an
analytic strategy that uses any other package.

### Times

Following ISO 8601-2:2019, `mdate` objects can also carry a time of day,
appended to a date with a space (as ISO 8601-1 and RFC 3339 both permit
as an alternative to `T`, and messydates uses for readability; `T`
continues to be accepted on input). Hours, minutes, and seconds (with
fractional seconds), am/pm times, the UTC designator `Z`, and numeric
offsets such as `+02:00` are all parsed and standardised. Time
components accept the same approximate (`~`), uncertain (`?`), and
unspecified (`X`) annotations as dates.

``` r

library(messydates)
as_messydate(c("2019-03-01 14:30:00Z", "2019-03-01 2:30pm", "2019-03-01 ~14:30"))
#>  'mdate' chr [1:3] "2019-03-01 14:30:00Z" "2019-03-01 14:30" ...
# extract and measure sub-day components
hour(as_messydate("2019-03-01 14:30:00"))
#> [1] 14
precision(as_messydate("2019-03-01 14:30")) # 1440 = 1/minute of a day
#> [1] 1440
# sub-day arithmetic and sequences
as_messydate("2019-03-01 14:30:00") + "2 hours"
#>  'mdate' chr "2019-03-01 16:30:00"
seq(as_messydate("2019-03-01 09:00"), as_messydate("2019-03-01 12:00"), by = "hour")
#> [1] "2019-03-01 09:00:00" "2019-03-01 10:00:00" "2019-03-01 11:00:00"
#> [4] "2019-03-01 12:00:00"
```

Because `:` also serves as a range separator, times are detected first,
so `2009-01-01:2019-01-01` is still read as a range. Sub-day arithmetic,
sequences, and coercion to and from `POSIXct`/`POSIXlt` all preserve the
time of day.

## Cheat Sheet

Please see the cheat sheet and [the messydates
website](https://globalgov.github.io/messydates/) for more information
about how to use [messydates](https://globalgov.github.io/messydates/).

[![messydates
cheatsheet](https://raw.githubusercontent.com/globalgov/messydates/main/man/figures/cheatsheet.png)](https://github.com/globalgov/messydates/blob/main/inst/figures/cheatsheet.pdf)

## Installation

The easiest way to install
[messydates](https://globalgov.github.io/messydates/) is directly from
CRAN:

``` r

install.packages("messydates")
```

However, you may also install the development version from
[GitHub](https://github.com/).

``` r

# install.packages("remotes")
remotes::install_github("globalgov/messydates")
```

## Funding

The package was developed as part of [the PANARCHIC
project](https://panarchic.ch), which studies the effects of network and
power on how quickly states join, reform, or create international
institutions by examining the historical dynamics of institutional
networks from different domains.

The PANARCHIC project is funded by the Swiss National Science Foundation
([SNSF](https://data.snf.ch/grants/grant/188976)). For more information
on current projects of the Geneva Global Governance Observatory, please
see [our Github website](https://github.com/globalgov).

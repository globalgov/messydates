
# messydates <img src="man/figures/messydates_hexlogo.png" align="right" width="220"/>

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
![CRAN/METACRAN](https://img.shields.io/cran/v/messydates) ![GitHub
release (latest by
date)](https://img.shields.io/github/v/release/globalgov/messydates)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/messydates)
<!-- ![GitHub issues](https://img.shields.io/github/issues-raw/globalgov/messydates) -->
[![Codecov test
coverage](https://codecov.io/gh/globalgov/messydates/branch/main/graph/badge.svg)](https://app.codecov.io/gh/globalgov/messydates?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/globalgov/messydates/badge)](https://www.codefactor.io/repository/github/globalgov/messydates)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/5061/badge)](https://bestpractices.coreinfrastructure.org/projects/5061)
<!-- badges: end -->

## Why this package?

Existing packages for working with dates in R expect them to be *tidy*.
That is, they should be in or coercible to the standard ISO format
`yyyy-mm-dd`.

But dates are often ***messy***. Sometimes we only know the year when
something happened, leaving other components of the date, such as the
month or day, *unspecified*. This is often the case with historical
dates, for instance. Sometimes we can only say *approximately* when an
event occurred, that it occurred *before* or *after* a certain date, or
we recognise that our best estimate comes from a *dubious* source. Other
times there exists a *set* or *range* of possible dates for an event.

Although researchers generally recognise this messiness, many feel
expected to force artificial precision or unfortunate imprecision on
temporal data to proceed with analysis. For example, if we only know
something happened in `2021`, then we might revert to a panel data
design *even if greater precision is available*, or opt to replace this
date with the start of that year (`2021-01-01`), assuming that erring on
the earlier (or later) side is more justifiable than a random date
within that month or year.

However, this can create inferential issues when timing or sequence is
important. `{messydates}` assists with this problem by retaining and
working with various kinds of date imprecision.

    #> 
    #> Attaching package: 'lubridate'
    #> The following objects are masked from 'package:messydates':
    #> 
    #>     day, month, year
    #> The following objects are masked from 'package:base':
    #> 
    #>     date, intersect, setdiff, union
    #> 
    #> Attaching package: 'dplyr'
    #> The following objects are masked from 'package:stats':
    #> 
    #>     filter, lag
    #> The following objects are masked from 'package:base':
    #> 
    #>     intersect, setdiff, setequal, union
    #> 
    #> Attaching package: 'kableExtra'
    #> The following object is masked from 'package:dplyr':
    #> 
    #>     group_rows

## A quick overview

`{messydates}` implements for R the extended annotation standard for
dates, the Extended Date/Time Format (EDTF) outlined in [ISO
8601-2_2019(E)](https://www.iso.org/standard/70908.html). `{messydates}`
introduces a new `mdate` class that embeds these annotations, and offers
a set of methods for constructing and coercing into and from the `mdate`
class, as well as tools for working with such ‘messy’ dates.

``` r
pkg_comparison <- tibble::tribble(~Example, ~OriginalDate,
                                    "Normal date", "2010-01-01",
                                    "Future date", "2599-12-31",
                                    "Written date", "First of February, two thousand and twenty-one",
                                    "Historical date", "476",
                                    "Era date", "33 BC",
                                    "Approximate date", "2012-01-12~",
                                    "Uncertain date", "2001-01-01?",
                                    "Unspecified date", "2012-01",
                                    "Censored date", "..2012-01-12", 
                                    "Range of dates", "2019-11-01:2020-01-01",
                                    "Set of dates", "2021-5-26, 2021-11-19, 2021-12-4") %>%
  dplyr::mutate(base = as.Date(OriginalDate),
                lubridate = lubridate::as_date(OriginalDate),
                messydates = messydates::as_messydate(OriginalDate))
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Example
</th>
<th style="text-align:left;">
OriginalDate
</th>
<th style="text-align:left;">
base
</th>
<th style="text-align:left;">
lubridate
</th>
<th style="text-align:left;">
messydates
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Normal date
</td>
<td style="text-align:left;">
2010-01-01
</td>
<td style="text-align:left;color: black !important;">
2010-01-01
</td>
<td style="text-align:left;color: black !important;">
2010-01-01
</td>
<td style="text-align:left;color: black !important;">
2010-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
Future date
</td>
<td style="text-align:left;">
2599-12-31
</td>
<td style="text-align:left;color: black !important;">
2599-12-31
</td>
<td style="text-align:left;color: black !important;">
2599-12-31
</td>
<td style="text-align:left;color: black !important;">
2599-12-31
</td>
</tr>
<tr>
<td style="text-align:left;">
Written date
</td>
<td style="text-align:left;">
First of February, two thousand and twenty-one
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: black !important;">
2021-02-01
</td>
</tr>
<tr>
<td style="text-align:left;">
Historical date
</td>
<td style="text-align:left;">
476
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: black !important;">
0476
</td>
</tr>
<tr>
<td style="text-align:left;">
Era date
</td>
<td style="text-align:left;">
33 BC
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: black !important;">
-0033
</td>
</tr>
<tr>
<td style="text-align:left;">
Approximate date
</td>
<td style="text-align:left;">
2012-01-12\~
</td>
<td style="text-align:left;color: red !important;">
2012-01-12
</td>
<td style="text-align:left;color: red !important;">
2012-01-12
</td>
<td style="text-align:left;color: black !important;">
2012-01-12\~
</td>
</tr>
<tr>
<td style="text-align:left;">
Uncertain date
</td>
<td style="text-align:left;">
2001-01-01?
</td>
<td style="text-align:left;color: red !important;">
2001-01-01
</td>
<td style="text-align:left;color: red !important;">
2001-01-01
</td>
<td style="text-align:left;color: black !important;">
2001-01-01?
</td>
</tr>
<tr>
<td style="text-align:left;">
Unspecified date
</td>
<td style="text-align:left;">
2012-01
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: red !important;">
2020-12-01
</td>
<td style="text-align:left;color: black !important;">
2012-01
</td>
</tr>
<tr>
<td style="text-align:left;">
Censored date
</td>
<td style="text-align:left;">
..2012-01-12
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: red !important;">
2012-01-12
</td>
<td style="text-align:left;color: black !important;">
..2012-01-12
</td>
</tr>
<tr>
<td style="text-align:left;">
Range of dates
</td>
<td style="text-align:left;">
2019-11-01:2020-01-01
</td>
<td style="text-align:left;color: red !important;">
2019-11-01
</td>
<td style="text-align:left;color: red !important;">
2019-11-01
</td>
<td style="text-align:left;color: black !important;">
2019-11-01..2020-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
Set of dates
</td>
<td style="text-align:left;">
2021-5-26, 2021-11-19, 2021-12-4
</td>
<td style="text-align:left;color: red !important;">
2021-05-26
</td>
<td style="text-align:left;color: red !important;">
NA
</td>
<td style="text-align:left;color: black !important;">
{2021-05-26,2021-11-19,2021-12-04}
</td>
</tr>
</tbody>
</table>

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
`{lubridate}`.

So only `{messydates}` enables researchers to retain all this
information. But most analysis does still expect some precision in dates
to work.

## Working with messy dates

The first way that `{messydates}` assists researchers that use dates in
`mdate` class is to provide methods for converting back into common date
classes such as `Date`, `POSIXct`, and `POSIXlt`. It is thus fully
compatible with packages such as `{lubridate}` and `{anydate}`.

As messy date annotations can indicate multiple possible dates,
`{messydates}` allows e.g. ranges or sets of dates to be unpacked or
expanded into all compatible dates.

Since most methods of analysis or modelling expect single date
observations, we offer ways to resolve this multiplicity when coercing
`mdate`-class objects into other date formats. For example, researcher
might explicitly choose to favour the `min()`, `max()`, `mean()`,
`median()`, or even a `random()` date. This greatly facilitates research
transparency by demanding a conscious choice from researchers, as well
as supporting robustness checks by enabling description or inference
across dates compatible with the messy annotated date.

``` r
resolve_mdate <- pkg_comparison %>% 
  dplyr::select(messydates) %>% 
  dplyr::mutate(min = as.Date(messydates, min),
         median = as.Date(messydates, median),
         max = as.Date(messydates, max))
#> Please specify 'approx_range' argument if you want approximate dates to also be expanded
#> Please specify 'approx_range' argument if you want approximate dates to also be expanded
#> Please specify 'approx_range' argument if you want approximate dates to also be expanded
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
messydates
</th>
<th style="text-align:left;">
min
</th>
<th style="text-align:left;">
median
</th>
<th style="text-align:left;">
max
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2010-01-01
</td>
<td style="text-align:left;">
2010-01-01
</td>
<td style="text-align:left;">
2010-01-01
</td>
<td style="text-align:left;">
2010-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
2599-12-31
</td>
<td style="text-align:left;">
2599-12-31
</td>
<td style="text-align:left;">
2599-12-31
</td>
<td style="text-align:left;">
2599-12-31
</td>
</tr>
<tr>
<td style="text-align:left;">
2021-02-01
</td>
<td style="text-align:left;">
2021-02-01
</td>
<td style="text-align:left;">
2021-02-01
</td>
<td style="text-align:left;">
2021-02-01
</td>
</tr>
<tr>
<td style="text-align:left;">
0476
</td>
<td style="text-align:left;">
0476-01-01
</td>
<td style="text-align:left;">
0476-07-02
</td>
<td style="text-align:left;">
0476-12-31
</td>
</tr>
<tr>
<td style="text-align:left;">
-0033
</td>
<td style="text-align:left;">
-033-01-01
</td>
<td style="text-align:left;">
-033-07-02
</td>
<td style="text-align:left;">
-033-12-31
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-01-12\~
</td>
<td style="text-align:left;">
2012-01-12
</td>
<td style="text-align:left;">
2012-01-13
</td>
<td style="text-align:left;">
2012-01-13
</td>
</tr>
<tr>
<td style="text-align:left;">
2001-01-01?
</td>
<td style="text-align:left;">
2001-01-01
</td>
<td style="text-align:left;">
2001-01-01
</td>
<td style="text-align:left;">
2001-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-01
</td>
<td style="text-align:left;">
2012-01-01
</td>
<td style="text-align:left;">
2012-01-16
</td>
<td style="text-align:left;">
2012-01-31
</td>
</tr>
<tr>
<td style="text-align:left;">
..2012-01-12
</td>
<td style="text-align:left;">
2012-01-12
</td>
<td style="text-align:left;">
2012-01-12
</td>
<td style="text-align:left;">
2012-01-12
</td>
</tr>
<tr>
<td style="text-align:left;">
2019-11-01..2020-01-01
</td>
<td style="text-align:left;">
2019-11-01
</td>
<td style="text-align:left;">
2019-12-02
</td>
<td style="text-align:left;">
2020-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
{2021-05-26,2021-11-19,2021-12-04}
</td>
<td style="text-align:left;">
2021-05-26
</td>
<td style="text-align:left;">
2021-11-19
</td>
<td style="text-align:left;">
2021-12-04
</td>
</tr>
</tbody>
</table>

As can be seen in the table above, all ‘precise’ dates are respected as
such, and returned no matter what ‘resolution’ function is given. But
for messy dates, the choice of function can make a difference. Where
only a year is given, e.g. `0476` or `-0033`, we draw from all the days
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

## Cheat Sheet

Please see the cheat sheet and [the messydates
website](https://globalgov.github.io/messydates/) for more information
about how to use `{messydates}`.

<a href="https://github.com/globalgov/messydates/blob/main/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/messydates/main/man/figures/cheatsheet.png" width="525" height="378"/></a>

## Installation

The easiest way to install `{messydates}` is directly from CRAN:

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
([SNSF](https://p3.snf.ch/Project-188976)). For more information on
current projects of the Geneva Global Governance Observatory, please see
[our Github website](https://github.com/globalgov).

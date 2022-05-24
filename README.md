
# messydates <img src="man/figures/messydates_hexlogo.png" align="right" width="220"/>

<!-- README.md is generated from README.Rmd. Please edit that file -->
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
something happened, leaving other components of the date *unspecified*.
This is often the case with historical dates, for instance. Sometimes we
can only say *approximately* when an event occurred, that it occurred
*before* or *after* a certain date, or we recognise that our best
estimate comes from a *dubious* source. Other times there exists a *set*
or *range* of possible dates for an event.

Although researchers generally recognise this messiness, many feel
expected to force artificial precision on temporal data to proceed with
analysis. For example, if we only know something happened in `2021`, we
might opt to replace this date with the start of that year
(`2021-01-01`), assuming that erring on the earlier (or later) side is
more justifiable than a random date within that month or year.

However, this can create inferential issues when sequence or timing is
important. `{messydates}` assists with this problem by retaining and
working with various kinds of date imprecision.

## A quick overview

`{messydates}` implements for R the extended annotation standard for
dates, the Extended Date/Time Format (EDTF) outlined in [ISO
8601-2_2019(E)](https://www.iso.org/standard/70908.html). These include
standardised notation for:

-   *unspecified* date( component)s, e.g. `2012-XX-01` for the first of
    some unknown month in 2012 or `2012-01` for some unknown day in
    January 2012
-   *approximate* date( component)s, e.g. `2012-01-12~` for
    approximately the 12th of January 2012
-   *uncertain* date( component)s, e.g. `2012-01-12?` where this data
    point is based on an unreliable source
-   *sets* of dates, e.g. `{2012-01-01,2012-01-12}` where the date can
    be both 1 January 2012 and 12 January 2012
-   *ranges* of dates, e.g. `2012-01-01..2012-01-12` for all dates
    between the 1 January 2012 and 12 January 2012 inclusive
-   *censored* dates, e.g. `..2012-01-01` for 1 January 2012 or before
    and `2012-01-01..` for 1 January 2012 or after

`{messydates}` introduces a new `mdate` class that embeds these
annotations, and offers a set of methods for constructing and coercing
into and from the `mdate` class, as well as tools for working with such
‘messy’ dates.

Importantly, the package also includes a function for unpacking or
expanding sets or ranges of dates into all dates consistent with how the
date or set of dates is specified or annotated. Methods are also offered
that can be used to make explicit how researchers convert date
imprecision into precise dates for analysis, such as getting the
`min()`, `max()`, or even a `random()` date from among the dates
consistent with a set or range of dates. This greatly facilitates
research transparency as well as robustness checks.

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

Please see [the messydates
website](https://globalgov.github.io/messydates/) for more information
about how to use `{messydates}`. The package was developed as part of
[the PANARCHIC project](https://panarchic.ch), which studies the effects
of network and power on how quickly states join, reform, or create
international institutions by examining the historical dynamics of
institutional networks from different domains. The PANARCHIC project is
funded by the Swiss National Science Foundation
([SNSF](https://p3.snf.ch/Project-188976)). For more information on
current projects of the Geneva Global Governance Observatory, please see
[our Github website](https://github.com/globalgov).

## Cheat Sheet

<a href="https://github.com/globalgov/messydates/blob/main/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/messydates/main/man/figures/cheatsheet.png" width="525" height="378"/></a>

## Relationship to other packages

`{messydates}` offers a new date class, but one that comes with methods
for converting from and into common date classes such as `Date`,
`POSIXct`, and `POSIXlt`. It is thus fully compatible with packages such
as `{lubridate}` and `{anydate}`.

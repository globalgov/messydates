
# messydates <img src="man/figures/messydates_hexlogo.png" align="right" width="220"/>

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

![GitHub release (latest by
date)](https://img.shields.io/github/v/release/globalgov/messydates)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/messydates)
![GitHub
issues](https://img.shields.io/github/issues-raw/globalgov/messydates)
[![Codecov test
coverage](https://codecov.io/gh/globalgov/messydates/branch/main/graph/badge.svg)](https://codecov.io/gh/globalgov/qCreate?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/globalgov/messydates/badge)](https://www.codefactor.io/repository/github/globalgov/messydates)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/5061/badge)](https://bestpractices.coreinfrastructure.org/projects/5061)
<!-- badges: end -->

## Why this package?

Dates are often messy. Whether historical (or ancient), future, or even
recent, we often only know approximately when an event occurred, that it
happened within a particular period, an unreliable source means a date
should be flagged as uncertain, or sources offer multiple, competing
dates.

As researchers, we often recognise this messiness but are forced to
force nonexistent precision on data so we can proceed with analysis. For
example, if we only know something happened in a given month or year, we
might just opt for the start of that month (e.g. 2021-07-01) or year
(2021-01-01), assuming that to err on the earlier (or later) side is a
justifiable bias. However, this can create issues for inference in which
sequence or timing is important. The goal of `{messydates}` is to help
with this problem by retaining and working with various kinds of date
imprecision.

## A quick overview

`{messydates}` implements the extended annotation standard for dates,
the Extended Date/Time Format (EDTF), outlined in [ISO
8601-2\_2019(E)](https://www.iso.org/standard/70908.html) for R. These
include standardised notation for:

-   unspecified date( component)s, e.g. `2012-XX-01` for the first of
    some unknown month in 2012 or `2012-01` for some unknown day in
    January 2012
-   approximate date( component)s, e.g. `2012-01-12~` for approximately
    the 12th of January 2012
-   uncertain date( component)s, e.g. `2012-01-12?` where this data
    point is based on an unreliable source
-   sets of dates, e.g. `{2012-01-01,2012-01-12}` where the date is
    either 1 January 2012 or 12 January 2012
-   ranges of dates, e.g. `2012-01-01..2012-01-12` for all dates between
    the 1 January 2012 and 12 January 2012 inclusive

`{messydates}` contains a set of tools for constructing and coercing
into and from the ‘messydt’ class. This date class allows regular dates
to be annotated to express unspecified date components, approximate or
uncertain date components, date ranges, and sets of dates.

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
the PANARCHIC project, which studies the effects of network and power on
how quickly states join, reform, or create international institutions by
examining the historical dynamics of institutional networks from
different domains. The PANARCHIC project is funded by the Swiss National
Science Foundation (SNSF). For more information on the Geneva Global
Governance Observatory and our current projects, please see [our
website](https://panarchic.ch) or [our Github
page](https://github.com/globalgov).

## Cheat Sheet

<a href="https://github.com/globalgov/messydates/blob/develop/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/messydates/develop/man/figures/cheatsheet.png" width="787" height="567"/></a>

## Relationship to other packages

`{messydates}` offers a new date class, but one that comes with methods
for converting from and into common date classes such as `Date`,
`POSIXct`, and `POSIXlt`. It is thus fully compatible with packages such
as `{lubridate}` and `{anydate}`. It is also compatible with
`{unstruwwel}`, which also parses historic dates in R, though the
emphasis of our package is on working with these dates.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# messydates

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![GitHub release (latest by
date)](https://img.shields.io/github/v/release/globalgov/messydates)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/messydates)
![GitHub
issues](https://img.shields.io/github/issues-raw/globalgov/messydates)
[![Codecov test
coverage](https://codecov.io/gh/globalgov/messydates/branch/main/graph/badge.svg)](https://codecov.io/gh/globalgov/qCreate?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/globalgov/messydates/badge)](https://www.codefactor.io/repository/github/globalgov/messydates)
<!-- badges: end -->

Dates are often messy. Whether historical (or ancient), future, or even
recent, we often only know approximately when an event occurred, there
may be some uncertainty about the date due to the source being
unreliable, or there are multiple, competing dates.

Too often researchers recognise this but force a nonexistent precision
on data so they can move forward with analysis. For example, if we only
know something happened in a given month or year, we might just opt for
the start of that month (e.g. 2021-07-01) or year (2021-01-01), assuming
that to err on the earlier (or later) side is a justifiable bias.
However, this can create issues for inference in which sequence, or
timing, is important. The goal of `{messydates}` is to help with this
problem by retaining and working with various kinds of date imprecision.

`{messydates}` implements the extended annotation standard for dates,
the Extended Date/Time Format (EDTF), outlined in [ISO
8601-2\_2019(E)](https://www.iso.org/standard/70908.html) for R. These
include standardised notation for:

-   unspecified date( component)s
-   approximate date( component)s
-   uncertain date( component)s
-   sets of dates
-   ranges of dates

`{messydates}` contains a set of tools for constructing and coercing
into and from the ‘messydt’ class. This date class allows regular dates
to be annotated to express unspecified date components, approximate or
uncertain date components, date ranges, and sets of dates.

## Installation

You can install the the development version of messydates from
[GitHub](https://github.com/).

``` r
# install.packages("remotes")
remotes::install_github("globalgov/messydates")
```

Please see [the messydates
website](https://globalgov.github.io/messydates) for more information
about how to use `{messydates}`. For more information on the Geneva
Global Governance Observatory and our current projects, please see [our
website](https://panarchic.ch) or [our Github
page](https://github.com/globalgov).

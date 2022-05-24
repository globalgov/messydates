
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

## A quick overview

`{messydates}` implements for R the extended annotation standard for
dates, the Extended Date/Time Format (EDTF) outlined in [ISO
8601-2_2019(E)](https://www.iso.org/standard/70908.html). `{messydates}`
introduces a new `mdate` class that embeds these annotations, and offers
a set of methods for constructing and coercing into and from the `mdate`
class, as well as tools for working with such ‘messy’ dates.

``` r
library(messydates)
library(lubridate)
library(tibble)
library(dplyr)
dates_comparison <- tibble::tribble(~Example, ~OriginalDate,
                                    "A normal date", "2010-01-01",
                                    "A historical date", "476",
                                    "A negative date", "33 BC",
                                    "A future date", "2599-12-31",
                                    "An unspecified date", "2012-XX-01",
                                    "An approximate date", "2012-01-12~",
                                    "An uncertain date", "2001-01-01?",
                                    "A range of dates", "2019-11-01:2020-01-01",
                                    "A set of dates", "2021-5-26, 2021-6-10, 2021-11-19, 2021-12-4",
                                    "A censored date", "..2012-01-12", 
                                    "A writen date", "First of February, two thousand and twenty-one") %>%
  dplyr::mutate(base = as.Date(OriginalDate),
                lubridate = lubridate::as_date(OriginalDate),
                messydates = messydates::as_messydate(OriginalDate))
knitr::kable(dates_comparison)
```

| Example             | OriginalDate                                   | base       | lubridate  | messydates                                    |
|:--------------------|:-----------------------------------------------|:-----------|:-----------|:----------------------------------------------|
| A normal date       | 2010-01-01                                     | 2010-01-01 | 2010-01-01 | 2010-01-01                                    |
| A historical date   | 476                                            | NA         | NA         | 0476                                          |
| A negative date     | 33 BC                                          | NA         | NA         | -0033                                         |
| A future date       | 2599-12-31                                     | 2599-12-31 | 2599-12-31 | 2599-12-31                                    |
| An unspecified date | 2012-XX-01                                     | NA         | 2020-12-01 | 2012-XX-01                                    |
| An approximate date | 2012-01-12\~                                   | 2012-01-12 | 2012-01-12 | 2012-01-12\~                                  |
| An uncertain date   | 2001-01-01?                                    | 2001-01-01 | 2001-01-01 | 2001-01-01?                                   |
| A range of dates    | 2019-11-01:2020-01-01                          | 2019-11-01 | 2019-11-01 | 2019-11-01..2020-01-01                        |
| A set of dates      | 2021-5-26, 2021-6-10, 2021-11-19, 2021-12-4    | 2021-05-26 | NA         | {2021-05-26,2021-06-10,2021-11-19,2021-12-04} |
| A censored date     | ..2012-01-12                                   | NA         | 2012-01-12 | ..2012-01-12                                  |
| A writen date       | First of February, two thousand and twenty-one | NA         | NA         | 2021-02-01                                    |

## Working with messy dates

‘messy’ dates allow for unpacking or expanding sets or ranges of dates
into all dates consistent with how dates are annotated. Methods are also
offered that can be used to make explicit how researchers convert date
imprecision into precise dates for analysis, such as getting the
`min()`, `max()`, `median()` or even a `random()` date from among the
dates consistent with a set or range of dates. This greatly facilitates
research transparency as well as robustness checks.

``` r
as.Date(dates_comparison$messydates, min)
#>  [1] "2010-01-01" "0476-01-01" "-033-01-01" "2599-12-31" "2012-01-01"
#>  [6] "2009-01-12" "2001-01-01" "2019-11-01" "2021-05-26" "2009-01-12"
#> [11] "2021-02-01"
as.Date(dates_comparison$messydates, max)
#>  [1] "2010-01-01" "0476-12-31" "-033-12-31" "2599-12-31" "2012-12-01"
#>  [6] "2015-01-12" "2001-01-01" "2020-01-01" "2021-12-04" "2012-01-12"
#> [11] "2021-02-01"
as.Date(dates_comparison$messydates, median)
#>  [1] "2010-01-01" "0476-07-02" "-033-07-02" "2599-12-31" "2012-07-01"
#>  [6] "2012-01-13" "2001-01-01" "2019-12-02" "2021-11-19" "2010-07-14"
#> [11] "2021-02-01"
as.Date(dates_comparison$messydates, random)
#>  [1] "2010-01-01" "0476-04-11" "-033-10-07" "2599-12-31" "2012-04-01"
#>  [6] "2013-12-29" "2001-01-01" "2019-11-23" "2021-12-04" "2009-06-09"
#> [11] "2021-02-01"
```

## Cheat Sheet

Please see the cheat sheet and [the messydates
website](https://globalgov.github.io/messydates/) for more information
about how to use `{messydates}`.

<a href="https://github.com/globalgov/messydates/blob/main/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/messydates/main/man/figures/cheatsheet.png" width="525" height="378"/></a>

## Relationship to other packages

`{messydates}` offers a new date class that comes with methods for
converting from and into common date classes such as `Date`, `POSIXct`,
and `POSIXlt`. It is thus fully compatible with packages such as
`{lubridate}` and `{anydate}`.

## PANARCHIC

The package was developed as part of [the PANARCHIC
project](https://panarchic.ch), which studies the effects of network and
power on how quickly states join, reform, or create international
institutions by examining the historical dynamics of institutional
networks from different domains. The PANARCHIC project is funded by the
Swiss National Science Foundation
([SNSF](https://p3.snf.ch/Project-188976)). For more information on
current projects of the Geneva Global Governance Observatory, please see
[our Github website](https://github.com/globalgov).

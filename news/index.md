# Changelog

## messydates 1.1.1

### Package

- Added a reverse dependency job to the PR checks workflow

### Coercion

- Added parsing of a day of the year, e.g. “the 103rd day of 2026”
  (closes [\#97](https://github.com/globalgov/messydates/issues/97))
  - Gives `"2026-04-13"`; `"2026-103"` notation is still rejected as
    ambiguous
- Added parsing of a week of the year, e.g. “the 5th week of 2026”
  (closes [\#96](https://github.com/globalgov/messydates/issues/96))
  - Gives the range `"2026-01-26..2026-02-01"`
  - Weeks follow ISO 8601, so week 1 can start in December of the year
    before
  - `"2026-W05"` and `"2026-W05-3"` are read too, but recorded as
    dates/ranges
- Updated the documentation of seasons in prose, e.g. “summer 2026”
  (closes [\#98](https://github.com/globalgov/messydates/issues/98))
  - Gives the northern-hemisphere month range `"2026-06..2026-08"`
  - `"2026-22"` season codes are still rejected as ambiguous
- Improved the errors for ordinal dates and season codes to name the
  prose form
- Fixed “before” and “after” keeping both bounds of a range that
  followed
  - “before summer 2026” gave `"..2026-06..2026-08"` and now gives
    `"..2026-06"`
- Fixed prose naming an impossible date becoming `NA` without a warning
  - “the 53rd week of 2025” now warns, as other unparseable text does

## messydates 1.1.0

CRAN release: 2026-07-27

### Package

- Improved package architecture documentation in
  `.github/CONTRIBUTING.md` so that human contributors and coding agents
  read the same notes
- Added a PR metadata job to the PR checks workflow, which verifies that
  the `DESCRIPTION` version is bumped, that the PR title names the new
  version, and that the PR description itemizes changes under subsection
  titles; these items are consequently dropped from the pull request
  template
- Fixed the website deploy job installing `Config/Needs/check` packages
  instead of `Config/Needs/website`

### Class

- Improved
  [`validate_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)
  to report which elements failed and what they contained, instead of
  naming only the rule that was broken
  - No longer passes a vector of empty strings as valid because a single
    element somewhere in it happened to contain a digit
- Assigning an unparseable value into an `mdate` with `[<-` or `[[<-`
  now reports what could not be parsed, rather than silently blanking
  the element
- Added [`unique()`](https://rdrr.io/r/base/unique.html) and
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html) methods for
  `mdate` objects (closes
  [\#106](https://github.com/globalgov/messydates/issues/106))
  - Previously [`unique()`](https://rdrr.io/r/base/unique.html) fell
    through to the character method and dropped the class, so a
    deduplicated column silently stopped being an `mdate`
  - Both compare the annotated strings rather than the dates they expand
    to, so `"2012-01"` and `"2012-01-01..2012-01-31"` remain distinct

### Coercion

- Improved
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  to validate its input rather than carrying values it cannot interpret:
  - dates with impossible components are rejected, so `"2019-02-30"`,
    `"2019-06-31"` and `"2019-01-01 25:00"` now error instead of being
    accepted as written; relatedly, `"2019-13-45"` is no longer silently
    reordered into `"2019-45-13"`, since month and day are only swapped
    where the swap yields a date that could exist
  - ISO 8601-2 notations that
    [messydates](https://globalgov.github.io/messydates/) does not
    represent (week dates, ordinal dates, season codes, significant
    digits, extended years, the `P`-style duration notation, and
    repeating intervals) now error naming the format, instead of passing
    through as strings that nothing downstream could expand, resolve or
    compare. Durations themselves are unaffected: they are written as
    date ranges and handled by `mduration`, and the error for `"P1Y2M"`
    points there
  - text naming no date at all still becomes `NA`, but now warns,
    listing what could not be read
- Added
  [`md_problems()`](https://globalgov.github.io/messydates/reference/md_problems.md),
  which reports one row per unparseable element of a vector, with the
  reason for each, for checking a column of dates before coercing it
- Added
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  methods for factors (coerced via their labels, the common case when a
  column was read in with `stringsAsFactors = TRUE`) and a default
  method that names the offending class rather than failing with R’s
  `UseMethod` message
- Improved parsing consistency across scalars and vectors
  - a month-first date such as `"July 4 1976"` previously lost its day
    whenever another value shared the vector with it
- Fixed how `[]` sets (“one member of”) silently rewrote as
  [`{}`](https://rdrr.io/r/base/Paren.html) sets (“all members of”).
  - Note that the two still expand and resolve alike; giving `[]` its
    own meaning in the resolution functions remains outstanding
- Fixed BCE/CE prose off-by-one error (closed
  [\#94](https://github.com/globalgov/messydates/issues/94), thanks
  [@njbart](https://github.com/njbart)) to convert to ISO 8601-2
  astronomical year numbering (proleptic Gregorian, in which a year zero
  exists and equals 1 BCE):
  - A historical `N BCE` maps to the astronomical year `-(N-1)`, so
    `"44 BC"` becomes `-0043` and `"1 BC"` becomes `0000` (year zero); a
    signed ISO year such as `"-0044"` is already astronomical and is
    left unchanged
  - Year zero is preserved on input (previously `0000` was misread as an
    unspecified year) and is traversed by
    [`seq()`](https://rdrr.io/r/base/seq.html) and
    [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md),
    so a sequence spanning the BCE/CE boundary now passes through the
    whole of year `0000` rather than jumping from `-0001` straight to
    `0001`
  - Era markers *on input* are now resolved for each year in an
    expression separately, rather than by counting how many markers a
    string contains (they are, as before, always removed in the parsed
    `mdate`, which records the era in the sign of the year alone). So an
    input marker written once at the end of a range or set applies to
    every bound of it: `"..200 BC"`, `"200..100 BC"` and `"44, 33 BC"`
    now give `..-199`, `-0199..-0099` and `{-0043,-0032}`, where
    previously the non-leading bounds silently stayed CE; an input
    marker written before a date still governs that date, so
    `"{BC2010-10-10,BC2010-10-11}"` gives `{-2009-10-10,-2009-10-11}`;
    and `"200 BC..100 AD"` spans the two eras, giving `-0199..0100`
  - Fixed how era markers could be dropped from approximate or uncertain
    dates given in prose, so `"circa 200 BC"` is now `~-199` rather than
    `0200~`

### Expand/Contract

- Fixed how
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  handles unspecified years
  - Previously `"192X"` raised `'from' must be a finite number` and
    `"18XX"` silently returned a single date, even though these are what
    the prose parser produces for decades and centuries
  - Now a bare unspecified year expands to the whole span (`"192X"`
    gives every day of the 1920s)
  - Now an attached month or day picks out that month or day in each
    candidate year (`"192X-05-04"` gives ten dates, not nine years of
    them)
  - Now BCE years are bounded the other way round, `-1999` being earlier
    than `-1900`
  - Now a year too vague to enumerate, such as `"XXXX"`, refused with a
    message suggesting
    [`vmin()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)/[`vmax()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
- Fixed how
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  applies unspecified-component rules to each member of a set
  separately, fixing over-expansion of sets whose members had an
  unspecified month: `"{2008-XX-31,2009-XX-31}"` gave 671 dates and now
  gives 24
- Fixed
  [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
  returning every set in [`{}`](https://rdrr.io/r/base/Paren.html)
  notation, so that a `[]` set no longer became a
  [`{}`](https://rdrr.io/r/base/Paren.html) set on a round-trip through
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)/[`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
  (closes [\#99](https://github.com/globalgov/messydates/issues/99))
  - Only applies where
    [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
    is given an `mdate`, since a list of dates does not record which
    kind of set its members came from
  - Documented what the two set types mean once resolved or operated on,
    having established that both should continue to expand to the same
    members (closes
    [\#99](https://github.com/globalgov/messydates/issues/99)):
    - [`?resolve_tendency`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
      now distinguishes a central tendency that describes where several
      recorded occurrences sit
      ([`{}`](https://rdrr.io/r/base/Paren.html)) from one that is a
      point estimate of a single unknown date (`[]`), as for a range
    - [`?operate_set`](https://globalgov.github.io/messydates/reference/operate_set.md)
      and
      [`?operate_proportional`](https://globalgov.github.io/messydates/reference/operate_proportional.md)
      now note that a result for a `[]` set reads as the candidates that
      remain possible, or the probability that a comparison holds,
      rather than a share of recorded occurrences

## messydates 1.0.0

CRAN release: 2026-07-16

### Package

- Updated the [messydates](https://globalgov.github.io/messydates/) logo
  to better reflect the package’s purpose and be more brand-consistent
  with manydata packages
- Removed `purrr` and `dplyr` dependencies (replaced with base R),
  leaving only `stringi` and `lubridate` as imports
- Updated the cheatsheet to reflect the new time support and other
  changes
- Declared `anytime` and `clock` in Suggests, since the interoperability
  tests exercise them where installed

### Class

- Added support for ISO 8601-2:2019 **times** of day in the `mdate`
  class
  - [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
    now parses an optional date-time separator, `hh`, `hh:mm`, and
    `hh:mm:ss` (with fractional seconds), am/pm times, the UTC
    designator `Z`, and numeric offsets (e.g. `+02:00`), zero-padding
    and normalising them
  - Dates and times are separated by a space
    (e.g. `2019-03-01 14:30:00`) for readability (as permitted by ISO
    8601-1 sec. 4.3.2 and RFC 3339), but a `T` separator can be used on
    input and is normalised to a space in the output
  - `:` and `_` continue to work as range separators; times are detected
    and protected first, so `2009-01-01:2019-01-01` is a range while
    `2019-03-01 14:30:00` is a time
  - Time components can carry the same annotations as dates: approximate
    (`~`), uncertain (`?`), both (`%`), and unspecified (`X`),
    e.g. `2019-03-01 ~14:30`
  - A time of day can now be given on its own, with no date part
    (e.g. `as_messydate("2:30pm")` -\> `14:30`,
    `as_messydate("around 2pm")` -\> `14:00~`); this requires a clear
    time signal (a colon-clock or am/pm), so a bare `2019` is still a
    year, and a bare am/pm hour (`2pm`) fills to `14:00`. A leading “at”
    (`"at 2:30pm"`, `"at around 2pm"`) is recognised and dropped.
    [`hour()`](https://lubridate.tidyverse.org/reference/hour.html)/[`minute()`](https://lubridate.tidyverse.org/reference/minute.html)/[`second()`](https://lubridate.tidyverse.org/reference/second.html)/[`tz()`](https://lubridate.tidyverse.org/reference/tz.html),
    [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md),
    [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md),
    and
    [`approximate()`](https://globalgov.github.io/messydates/reference/component_annotate.md)/[`uncertain()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
    all handle date-less times
- Renamed
  [`messyduration()`](https://globalgov.github.io/messydates/reference/defunct.md)
  to
  [`make_messyduration()`](https://globalgov.github.io/messydates/reference/class_mduration.md)
  for consistency with other `make_*()` functions, and renamed resulting
  class from `messyduration` to `mduration`
- Fixed bug where a messyrange would shift instead of widen upon
  approximation

### Coercion

- Improved parsing of written dates (closed
  [\#52](https://github.com/globalgov/messydates/issues/52)):
  - Ordinal days (`"4th July 1976"`): either day-first or month-first
    order (`"Fourth of July 1976"`, `"July 4th 1976"`), “day of”
    phrasings, and “last day of `<month>`” (leap-year aware for
    February)
  - Connectives:
    - `"between the 13th and 15th of Feb 1977"` (or
      `"from the 13th to the 15th"`) becomes a range
    - `"the 13th or the 15th"` a set
    - plain `"13th and 15th"`, or a comma-separated list of dates,
      becomes several dates
  - Reduced-precision expressions:
    - month-and-year (`"February 2004"` -\> `2004-02`)
    - decades (`"the 1910s"` -\> `191X`)
    - centuries (`"the 19th century"` -\> `18XX`)
  - Open ranges: `"before 1910"` -\> `..1910` and `"after 1910"` -\>
    `1910..`, where the bound may itself be imprecise
    (`"before the 1920s"` -\> `..192X`)
  - Prose qualifiers:
    - approximate words (`"around"`, `"circa"`, …) add `~`
    - uncertain words (`"possibly"`, `"reportedly"`, …) add `?`
    - both together add `%`, applied to the most specific component
  - Roman numerals (e.g. `"MDCCLXXVI"` becomes `1776`)
  - Roman calendar references, e.g. `"the Ides of March, 44 BC"` becomes
    `-0044-03-15` (Kalends, Nones, and Ides, with the later Nones/Ides
    of March, May, July, and October)
- Coercion from `POSIXct`/`POSIXlt` now preserves the time of day
  (midnight is treated as date-only);
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html)/[`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html)
  restore it
- Improved interoperability with
  [lubridate](https://lubridate.tidyverse.org): its `as_date()` and
  `as_datetime()` coercion verbs now work on an `mdate`, honouring the
  `FUN` resolver (e.g. `as_date(md, FUN = vmax)`), as do
  [`format()`](https://rdrr.io/r/base/format.html) on `mdate` columns in
  data frames and tibbles
- Fixed
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html)/[`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html)
  erroring on a vector of two or more `mdate`s (the check for
  pre-common-era dates was not vectorised)

### Annotation

- Renamed
  [`as_approximate()`](https://globalgov.github.io/messydates/reference/defunct.md)/[`as_uncertain()`](https://globalgov.github.io/messydates/reference/defunct.md)
  to
  [`approximate()`](https://globalgov.github.io/messydates/reference/component_annotate.md)/[`uncertain()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  for consistency with
  [`on_or_before()`](https://globalgov.github.io/messydates/reference/component_annotate.md)/[`on_or_after()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  (these aren’t coercion functions); old names are defunct and warn
- Improved
  [`approximate()`](https://globalgov.github.io/messydates/reference/component_annotate.md)/[`uncertain()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  to accept “hour”, “minute”, “second”, and “time” components
- Improved
  [`approximate()`](https://globalgov.github.io/messydates/reference/component_annotate.md)/[`uncertain()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  to combine annotations as `%` when both are applied to the same
  component

### Resolution

- Fixed how
  [`median()`](https://rdrr.io/r/stats/median.html)/[`vmedian()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  treat an even number of dates, which no longer silently returns `NA`
  but instead averages the two middle values
- Fixed how
  [`mean()`](https://rdrr.io/r/base/mean.html)/[`vmean()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)/[`median()`](https://rdrr.io/r/stats/median.html)/[`vmedian()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  average precise date-times, honouring the time of day, instead of
  miscomputing via
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)

### Operations

- Improved arithmetic (`+`/`-`) and
  [`seq()`](https://rdrr.io/r/base/seq.html) to accept sub-day units
  (“hours”, “minutes”, “seconds”) and operate on times
  - [`messyduration()`](https://globalgov.github.io/messydates/reference/defunct.md)
    keeps sub-day precision
- Improved how arithmetic treats time of day and shifts the calendar
  components, so `"2012-02-03 14:30" + "1 year"` is `2013-02-03 14:30`
  (with month-end rollback)
- Fixed bug where adding/subtracting from an open-ended range
  (`"2012-01-01T09:00.." + "2 hours"` or
  `"..2012-01-01T09:00" + "1 month"`) dropped `..` marker
- Fixed how `<`/`>`/`<=`/`>=` compare time of day on the same calendar
  day, no longer silently truncating both sides to a date first and
  treating them as equal
- Fixed two related bugs
  ([\#92](https://github.com/globalgov/messydates/issues/92)) affecting
  *any* comparison of two `Date`/`POSIXct` objects in a session with
  [messydates](https://globalgov.github.io/messydates/) loaded,
  including in unrelated packages:
  - since `<`/`>`/`<=`/`>=` are registered for `"Date"`/`"POSIXt"`
    classes (so that e.g. `Date < mdate` works), *any* comparison of a
    zero-length or all-`NA` `Date`/`POSIXct`/`POSIXlt` value was passed
    through
    [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md),
    where [`ifelse()`](https://rdrr.io/r/base/ifelse.html) and
    [`paste0()`](https://rdrr.io/r/base/paste.html) silently changed
    type or length and tripped an internal
    [`is.character()`](https://rdrr.io/r/base/character.html) check.
    This broke loading packages whose `.onLoad` hooks compare
    timestamps, such as [httr2](https://httr2.r-lib.org)’s cache
    pruning, which in turn broke
    [`pkgdown::build_news()`](https://pkgdown.r-lib.org/reference/build_news.html)
  - when one side of a comparison had a time of day and the other did
    not, their numeric bounds were computed in different units
    (e.g. seconds, days) without converting to a common unit, which
    could silently reverse the result. In particular,
    `Sys.time() < (Sys.time() + Inf)` – the pattern
    [httr2](https://httr2.r-lib.org) uses to represent an unbounded
    retry deadline – incorrectly evaluated to `FALSE`, in turn breaking
    [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
    (and so any request made while
    [messydates](https://globalgov.github.io/messydates/) is loaded,
    including `pkgdown`’s GitHub release-timeline lookup)

### Extraction

- Added [`hour()`](https://lubridate.tidyverse.org/reference/hour.html),
  [`minute()`](https://lubridate.tidyverse.org/reference/minute.html),
  [`second()`](https://lubridate.tidyverse.org/reference/second.html),
  and [`tz()`](https://lubridate.tidyverse.org/reference/tz.html) time
  component extractors
- Improved
  [`month()`](https://lubridate.tidyverse.org/reference/month.html),
  [`day()`](https://lubridate.tidyverse.org/reference/day.html), and
  [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  to be vectorised and no longer call
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  more than necessary
- Improved
  [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  to extend below the day: 24 to the hour, 1440 to the minute, and 86400
  to the second (date-level precision is unchanged)
- [`year()`](https://lubridate.tidyverse.org/reference/year.html),
  [`month()`](https://lubridate.tidyverse.org/reference/month.html),
  [`day()`](https://lubridate.tidyverse.org/reference/day.html),
  [`hour()`](https://lubridate.tidyverse.org/reference/hour.html),
  [`minute()`](https://lubridate.tidyverse.org/reference/minute.html),
  [`second()`](https://lubridate.tidyverse.org/reference/second.html),
  and [`tz()`](https://lubridate.tidyverse.org/reference/tz.html) are
  now S3 methods on the same-named
  [lubridate](https://lubridate.tidyverse.org) generics rather than
  plain functions, so the two packages can be loaded together (in either
  order) without one masking the other: these accessors dispatch to the
  messy-date logic on an `mdate` but to
  [lubridate](https://lubridate.tidyverse.org)’s own methods on a
  `Date`/`POSIXct`

### Expand/Contract

- Fixed
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  error when `approx_range` was set and the vector contained a
  reduced-precision value (e.g. a bare year-month) alongside an
  approximate one
- Improved
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  with a `by` argument (default `"day"`)
  - Ranges are enumerated at day granularity to avoid combinatorial
    explosion; set `by` to `"hour"`, `"min"`, or `"sec"` for finer
    enumeration
  - Precise date-times keep their time

## messydates 0.5.4

CRAN release: 2025-06-02

### Coercion

- Improved how `as_messydates()` handles text with dates in American
  format, e.g. October 10, 2010 (fixes
  [\#86](https://github.com/globalgov/messydates/issues/86))

## messydates 0.5.3

CRAN release: 2025-03-20

### Components

- Improved
  [`year()`](https://lubridate.tidyverse.org/reference/year.html) to be
  faster and work on durations
- Improved
  [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  - [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
    is now a S3 generic, dispatching to
    [`precision.mdate()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  - [`precision.mdate()`](https://globalgov.github.io/messydates/reference/component_extract.md)
    now returns the inverse of the previous measure, meaning maximising
    precision makes more sense

## messydates 0.5.2

CRAN release: 2025-03-07

### Package

- Moved `mreport()` to [manydata](https://www.manydata.ch/)
- Consolidated and renamed scripts internally

### Coerce to

- Fixed pkgdown#2855 by fixing how as_messydate methods interpret
  infinite dates
- Fixed time zone defaults in
  [`as.POSIXct.mdate()`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  and
  [`as.POSIXlt.mdate()`](https://globalgov.github.io/messydates/reference/coerce_from.md)
- Fixed set bug in
  [`validate_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)

### Coerce from

- Renamed [`as.numeric()`](https://rdrr.io/r/base/numeric.html) to
  [`as.double()`](https://rdrr.io/r/base/double.html) to fix S3
  dispatching
- Separated extrema functions into
  [`min.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  and
  [`max.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  for summaries and
  [`vmin.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  and
  [`vmax.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  for vector coercion
- Separated tendency functions into
  [`mean.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  [`median.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  and
  [`modal.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  for summaries and
  [`vmean.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  [`vmedian.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  and
  [`vmodal.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  for vector coercion
- Vector coercion previously in
  [`random.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  now in
  [`vrandom.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
- Improved how coercion/resolution functions handle BCE dates

### Manipulation

- Fixed how
  [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  calculates precision

## messydates 0.5.1

### Package

- Fixed redirected url in README
- Fixed pkgdown help links

## messydates 0.5.0

### Package

- Dropped the vignettes as they were ‘outdated’
- Dropped a number of tests to provide rapid testing framework
- Dropped [tibble](https://tibble.tidyverse.org/) dependency by just
  using [dplyr](https://dplyr.tidyverse.org)
- Updated Github workflows
- Updated testthat to version 3, tests now run in parallel
- Updated pkgdown to bootstrap 5
- Updated DESCRIPTION with config packages

### Functions

- Moved from [stringr](https://stringr.tidyverse.org) to
  [stringi](https://stringi.gagolewski.com/) for *speed*
- Added `as.numeric.mdate()` and
  [`as_messydate.numeric()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  for coercing between messydates and numbers (closes
  [\#85](https://github.com/globalgov/messydates/issues/85))
- Added
  [`seq.mdate()`](https://globalgov.github.io/messydates/reference/convert_sequence.md)
  for creating sequences from one or two messydates
  - This includes correct sequences for leap years and historical dates
    including before the common era
- Added
  [`is_bce()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  for testing whether dates are from before the common era
- Added `stri_squish()` helper for trimming white space everywhere
- Improved
  [`c.mdate()`](https://globalgov.github.io/messydates/reference/class_methods.md)
  so that it will strip class from an `mdate` object, as expected
- Improved
  [`is_uncertain()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  and
  [`is_approximate()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  so that they also recognise `%` annotations
- Improved
  [`min.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md),
  [`max.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md),
  and
  [`modal.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  to avoid using
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  and consequently run much faster
- Improved
  [`min.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md),
  [`max.mdate()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md),
  [`modal.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  [`mean.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  [`median.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md),
  and
  [`random.mdate()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  by adding `recursive` argument for resolving vectors down to a scalar
- Fixed bug in `messyduration.mdate()` where the minimum of an
  underspecified later date was used
- Fixed bug in
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  where zero padding for early dates was not added correctly

## messydates 0.4.1

CRAN release: 2024-04-19

### Package

- The package now depends on R versions bigger or equal to 4.0 since
  functions for subsetting and comparing ‘mdate’ objects rely on
  functions introduced in that version

### Functions

- Closed [\#83](https://github.com/globalgov/messydates/issues/83) by
  fixing how logical comparisons works for negative and year only dates

## messydates 0.4.0

### Functions

- Closed [\#46](https://github.com/globalgov/messydates/issues/46) by
  adding the `mdates_duration` class that introduces methods to annotate
  a duration or period with representations of its uncertainty
- Closed [\#72](https://github.com/globalgov/messydates/issues/72) by
  fixing issues with double unspecified components not being contracted
  correctly
- Closed [\#73](https://github.com/globalgov/messydates/issues/73) by
  fixing bugs with the conversion of dates where month is spelled
- Closed [\#74](https://github.com/globalgov/messydates/issues/74) and
  [\#82](https://github.com/globalgov/messydates/issues/82) by adding
  other logical comparison operators for ‘mdate’ objects (e.g. `<`, `>`,
  `<=`, `>=`) (thanks [@WerthPADOH](https://github.com/WerthPADOH))
- Closed [\#76](https://github.com/globalgov/messydates/issues/76) by
  adding proportional operators that calculate the proportion of messy
  dates meeting logical tests (e.g. `%l%` `%le%`, `%g%`, `%ge%`, `%><%`,
  `%>=<%`)
- Closed [\#77](https://github.com/globalgov/messydates/issues/77) by
  adding basic vector methods for subsetting and data frames (thanks
  [@WerthPADOH](https://github.com/WerthPADOH))
- Added alias function
  [`mdate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  for
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
- Renamed set family of functions to work as operators
  (i.e. `%intersect%` and `%union%`)
- Replaced
  [`is_element()`](https://globalgov.github.io/messydates/reference/defunct.md)
  by
  [`is_subset()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  for clarity and consistency
- Closed [\#80](https://github.com/globalgov/messydates/issues/80) by
  updating `make_messydates()` function to also construct ranges of
  dates

## messydates 0.3.5

CRAN release: 2023-01-20

### Functions

- Updated how
  [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
  function checks if ‘mdate’ object has been expanded

## messydates 0.3.4

### Package

- Updated ‘battles’ internal data
  - Corrected issues with zero padding for certain date ranges
  - Added ‘US_party’ and ‘N_actors’ additional variables for replication
    purposes

### Functions

- Closed [\#68](https://github.com/globalgov/messydates/issues/68) by
  updating
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  function
  - Fixed bugs with zero padding for ranges of dates
  - Fixed bugs with the re-ordering of months and day components for
    incorrectly specified dates
- Closed [\#69](https://github.com/globalgov/messydates/issues/69) by
  updating
  [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
  function to ‘expand’ dates before ‘contracting’ them
- Updated
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  function to handle, and properly convert, date objects that are not
  ‘mdate’

## messydates 0.3.3

CRAN release: 2022-12-20

### Package

- Moved cheatsheet.pdf to ‘inst’ folder instead of the ‘man’ folder

## messydates 0.3.2

### Package

- Closed [\#64](https://github.com/globalgov/messydates/issues/64) by
  updating failing tests to test for other aspects instead of the
  printing of negative dates across OS
- Closed [\#65](https://github.com/globalgov/messydates/issues/65) by
  updating cheatsheet for new package changes

### Functions

- Closed [\#62](https://github.com/globalgov/messydates/issues/62) by
  adding “resequence” as an argument to `as_messydates()` for explicit
  date format conversion, if necessary
- Closed [\#63](https://github.com/globalgov/messydates/issues/63) by
  fixing issues with unnecessary white spaces added in date conversion

## messydates 0.3.1

CRAN release: 2022-07-21

### Package

- Updated README by removing unattractive package startup messages
- Updated `battles` data by adding ‘parties’ variable

### Functions

- Closed [\#54](https://github.com/globalgov/messydates/issues/54) by
  adding new `mreport()` function to properly report on data containing
  ‘mdate’ variables
- Updated
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  function
  - Fixed bug with the expansion of approximate dates
  - Removed unnecessary function message

## messydates 0.3.0

CRAN release: 2022-06-02

### Package

- Closed [\#51](https://github.com/globalgov/messydates/issues/51) by
  changing object class name to `mdate`
  - Note that this is a *breaking* change
- Closed [\#41](https://github.com/globalgov/messydates/issues/41) by
  creating [skimr](https://docs.ropensci.org/skimr/) template for
  `mdate` class

### Functions

- Updated coercion to messy dates
  - Closed [\#26](https://github.com/globalgov/messydates/issues/26) by
    adding “resequence” argument to
    [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
    allowing users to choose component order of ambiguous dates
  - Closed [\#45](https://github.com/globalgov/messydates/issues/45) by
    improving how
    [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
    re-orders 6 digit date components if necessary
  - Closed [\#48](https://github.com/globalgov/messydates/issues/48) by
    adding zero padding incomplete date ranges and sets of dates
  - Updated
    [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
    to also extract dates from text strings
- Added
  [`is_precise()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  function that provides a logical test for precise dates
- Updated messy dates expansion
  - Updated
    [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
    to allow for the expansion of incomplete date ranges and sets of
    dates
  - Closed [\#49](https://github.com/globalgov/messydates/issues/49) by
    updating resolve functions to only expand dates if they are not
    precise

## messydates 0.2.1

CRAN release: 2022-02-23

### Package

- Added a vignette for working with the
  [messydates](https://globalgov.github.io/messydates/) package

### Functions

- Closed [\#9](https://github.com/globalgov/messydates/issues/9) by
  adding arithmetic operations for working with `messydt` objects
  - Added S3 methods for “+” and “-” operators
  - Added `add()` and `subtract()` helper functions for arithmetic
    operations
- Updated
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  function
  - Closed [\#31](https://github.com/globalgov/messydates/issues/31) by
    updating how approximate dates are expanded to account for leap
    years
  - Closed [\#34](https://github.com/globalgov/messydates/issues/34) by
    updating
    [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
    to manage negative dates
  - Added `expand_negative_dates()` helper function for expanding ranges
    of negative dates
- Updated functions that coerce from `messydt` objects to `Date` to
  manage negative dates
  - Added `negative_dates()` helper function to coerce negative
    `messydt` dates
- Closed [\#39](https://github.com/globalgov/messydates/issues/39) by
  updating how resolve mean methods work for negative dates
- Closed [\#40](https://github.com/globalgov/messydates/issues/40) by
  updating contract function to manage the contraction of negative dates
  - Added `compact_negative_dates()` helper function to compact negative
    date ranges
  - Added `is.sequence()` as a helper function to check if dates are a
    range
- Updated resequence script to export `interleave()` function

## messydates 0.2.0

CRAN release: 2021-11-12

### Package

- Added PANARCHIC project details to README file
- Added cheatsheet
- Added a new CSS style to website and updated functions displayed
- Addressed workflow actions issues
  - Updated pushrelease.yml workflow actions file to stop installing
    [messydates](https://globalgov.github.io/messydates/) from Github
  - Updated README file to stop installing
    [messydates](https://globalgov.github.io/messydates/) from Github
- Fixed Codecov test coverage URL on README file for CRAN submission

### Functions

- Expanded on messydates checks for class validity
- Fixed bugs for
  [`make_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)
- Added annotation functions and standardized annotation so that it is
  consistent with ISO2019E standards.
  - [`on_or_before()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  - [`on_or_after()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  - [`as_approximate()`](https://globalgov.github.io/messydates/reference/defunct.md)
  - [`as_uncertain()`](https://globalgov.github.io/messydates/reference/defunct.md)
    (includes discrimination between month uncertainty and day and month
    uncertainty)
- Updated
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  by adding zero padding for month, day or year
- Updated `resequence()` to work consistently with messydate objects
- Updated
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  function to expand imprecise, unspecified, approximate, uncertain, and
  negative dates according to approximate ranges and added tests
- Updated
  [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  to return the lengths of expanded dates
- Updated [`median()`](https://rdrr.io/r/stats/median.html) in resolve
  family of functions to work with changes to
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
- Added tests for functions
  - Added tests for
    [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  - Added tests for
    [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
  - Added tests for
    [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  - Added tests for `coerce_from_messydate()`

## messydates 0.1.1

CRAN release: 2021-07-19

### Package

- Updated README with some more explanation about what the package
  does/offers
- Fixed URL to the package website

## messydates 0.1.0

### Package

- Updated call to `messydt` class in DESCRIPTION file

### Functions

- Updated documentation for
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  functions
- Updated documentation for
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  function
- Updated documentation for resolve family of functions
- Updated documentation for coerce from family of functions
- Updated documentation for coerce to family of functions

## messydates 0.0.1

### Package

- Setup [messydates](https://globalgov.github.io/messydates/) package
  - Added `DESCRIPTION` file
  - Added `R` folder
  - Added `LICENSE` file
  - Added `NAMESPACE` file
  - Added `NEWS` file
  - Added `README` files
  - Added `.github` folder and files
  - Added `tests` folder and files
- Setup pkgdown website
- Added package logo

### Functions

- Added a new `messydt` class which follows the latest ISO 8601 (2019)
  standards
  - Added validation checks for messydt class
  - Added print methods for messydt class
- Added
  [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  function to coerce from date objects to messydate
  - [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
    standardises date order, separators and ambiguity
  - Added date class coercion
  - Added POSIXct class coercion
  - Added POSIXlt class coercion
  - Added character class coercion
- Added functions to coerce from messydate objects to other date classes
  - Added `as.Date.messydt()` for coercing to date class
  - Added `as.POSIXct.messydt()` for coercing to POSIXct class
  - Added `as.POSIXlt.messydt()` for coercing to POSIXlt class
- Added
  [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  function for expanding ranged and uncertain dates
- Added functions to resolve expanded dates
  - Added `min.messydt()` to get minimum value from expanded range
  - Added `max.messydt()` to get maximum value from expanded range
  - Added `median.messydt()` to get median value from expanded range
  - Added `mean.messydt()` to get mean value from expanded range
  - Added `modal.messydt()` to get mode value from expanded range
- Added
  [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
  function for contracting expanded dates
- Added extract functions to get particular date components
  - Added
    [`year()`](https://lubridate.tidyverse.org/reference/year.html) to
    extract year from date
  - Added
    [`month()`](https://lubridate.tidyverse.org/reference/month.html) to
    extract month from date
  - Added [`day()`](https://lubridate.tidyverse.org/reference/day.html)
    to extract day from date
- Added
  [`make_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)
  function to get messy dates from multiple columns
- Added set functions for operations in sets of messy dates
  - Added
    [`md_intersect()`](https://globalgov.github.io/messydates/reference/defunct.md)
    to find intersection of sets of messy dates
  - Added
    [`md_union()`](https://globalgov.github.io/messydates/reference/defunct.md)
    to find union of sets of messy dates
  - Added
    [`md_multiset()`](https://globalgov.github.io/messydates/reference/defunct.md)
    to join two sets of messy dates
- Added logical function for various logical tests for messy date
  objects
  - Added
    [`is_messydate()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
    to test for messydt class
  - Added
    [`is_intersecting()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
    to test if dates intersect
  - Added
    [`is_element()`](https://globalgov.github.io/messydates/reference/defunct.md)
    to test for multiple elements in dates
  - Added
    [`is_similar()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
    to test for similarities in dates
- Added tests for new functions
  - Added tests for messydt class and `às_messydate()` function
  - Added tests for coerce from messy dates functions
  - Added tests for coerce to messy dates functions
  - Added tests for
    [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
    function
  - Added tests for
    [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
    function
  - Added tests for extract functions
  - Added tests for
    [`make_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)
    function
  - Added tests for resolve functions
  - Added tests for set functions

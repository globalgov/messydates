# messydates 1.2.0

## Package

- Added `vctrs` and `graphics` to Imports and `tibble` to Suggests
- Added `data-raw/benchmark.R` to time parsing and resolution
- Improved `pushrelease.yml` to take the version number from `DESCRIPTION`
- Updated `CONTRIBUTING.md` with naming rules
- Improved the website reference with Parsing and Plotting sections
- Added equivalence test to compare parsing, expansion, and resolution with reference

## Class

- Improved `mdate` and `mduration` to be built on `vctrs`
  - `class(x)` is now `c("mdate", "vctrs_vctr")`
  - Columns keep their class in data frames, tibbles, `dplyr`, and `tidyr`
  - Tibbles show the column type as `<mdate>` or `<mdur>`
  - `c()` combines `mdate` with character, `Date`, and `POSIXct` values
  - `c(x)` on a single `mdate` keeps its class
  - Text that names no date is still an error in `c()` and `[<-`
- Accelerated `[`, `[[`, `[<-`, `[[<-`, `c()`, `rep()`, and `unique()`
  - The `vctrs` methods replace hand-written ones that parsed each value again
  - `as.list()` and `duplicated()` also come from `vctrs` now
  - More than 1000x faster for `c(x, x)` and `rep(x, 3)` on 10,000 values
  - ~175x faster for `x[1:100] <- y` on 10,000 values
  - More than 3x faster for `x[i]` and `unique(x)` on 10,000 values
- Added `{}` as the empty set, a value that is known to contain no date
  - ISO 8601-2 requires a set to have a member; `{}` extends the standard
  - Added `date_is_empty()` to test for it; `NA` is not the empty set
  - `possible_dates()` gives no dates for it
  - `vmin()`, the other `v*()` functions, and `precision()` give `NA`
  - `min()`, `max()`, `mean()`, `median()`, `modal()`, and `random()` leave it out
  - `tag_approximate()`, `tag_uncertain()`, `+`, and `-` give `{}` again

## Parsing

- Renamed the annotation functions to `tag_*()`
  - `approximate()` is now `tag_approximate()`
  - `uncertain()` is now `tag_uncertain()`
  - `on_or_before()` is now `tag_before()`
  - `on_or_after()` is now `tag_after()`
- Changed `tag_approximate()` and `tag_uncertain()` to tag the finest component by default
  - `tag_approximate("2012-06-15")` gives `"2012-06-~15"`, not `"2012-06-15~"`
  - Added `component = "date"` to tag the whole date
- Improved `tag_approximate()` and `tag_uncertain()` to write the fewest marks
  - A tag that is already there is not added again: `tag_approximate("~2012")` gives `"~2012"`
  - A wider tag replaces the marks that it covers: `tag_approximate("2012-~06", "date")` gives `"2012-06~"`
  - `~` and `?` on the same component become `%`
  - Each end of a range and each member of a set is tagged
- Fixed `tag_uncertain("~2012", "day")` giving `"~2012-XX-?NA"`; a missing component is not tagged
- Added `tag_unspecified()` to replace the digits of a component with `X`
- Added `untag()` to remove `~`, `?`, `%`, and the `..` of an open range
- Improved `tag_before()` and `tag_after()` on a closed range to keep the bounding end
  - `tag_before("2010..2012")` gives `"..2012"`, not `"..2010..2012"`
  - A value that is already open, and a set, stay as they are
- Fixed missing values in `tag_approximate()` and `tag_uncertain()` to give `NA`
- Fixed `as_messydate()` on a list to give an `mdate` vector, not a list
  - An empty element gives `{}`, and an element that is only `NA` gives `NA`
  - Added `collapse = FALSE` to keep a range that covers a whole month or year
- Accelerated `as_messydate()` (closes #105)
  - Prose and time steps run only on elements that can hold prose or a time
  - Prose that occurs more than once in a vector is interpreted once
  - Range and set members are padded together
  - ~9x faster for 6,000 plain ISO dates
  - ~45x faster for 6,000 mixed ISO, prose, and range values
- Improved the prose parser of `as_messydate()` (closes #102)
  - Added `lang = "fr"` to read French prose, e.g. `"vers 1910"` or `"mai 1997"`
    - Reads the same forms as English: qualifiers, open ranges, and connectives
    - Also decades, centuries (`"XIXe siècle"`), eras (`"av. J.-C."`), and seasons
    - Accents may be left out; spelled-out numbers are read in English only
  - Uses one table of English month names
  - Fixed lower-casing by the session locale, e.g. `"APRIL"` under Turkish
  - Fixed a century before the common era dropping its era
    - `"the 5th century BC"` gave `"04XX"` and now gives `"-04XX"`
- Improved `as_messydate()` on numbers from spreadsheets
  - Added `origin` to read serial day counts
    - `origin = "excel"` reads the 1900 date system of Excel
    - `origin = "excel1904"` reads the 1904 system of older Excel for Mac
    - Any other date is the day that counts as zero
    - A fractional part becomes a time of day
    - Serial 60, the 29 February 1900 of Excel, becomes `NA` with a warning
  - Fixed serial numbers kept as text: `as_messydate(36892)` gave `"36892"`
    - Without `origin`, whole numbers up to 9999 are still years
    - Valid eight-digit numbers are read as YYYYMMDD
    - Other numbers become `NA` with a warning that names `origin`
- Improved `as_messydate()` on two-digit years
  - Fixed two-digit years placed in the first century
    - `"1/1/29"` gave `"0001-01-29"` and now gives `"XX29-01-01"`
    - `"010221"` with `resequence = "dmy"` gave `"0021-02-01"`
  - Added `pivot` to place two-digit years in a 100-year window, e.g. `1930`
  - Fixed a month name with a two-digit year, as Excel shows a month
    - `"May-97"` gave an error and now gives `"XX97-05"`
  - Dates with slashes and a two-digit last component are read year last
  - A two-digit year written first (`"39-01-01"`) is still read as written
- Fixed a qualifier on a prose range marking only the last end
  - `"approximately 2019-01-01..2019-01-05"` gave `"2019-01-01..2019-01-05~"`
  - `"circa 1990..1995"` gave `"~1995"` and now gives `"~1990..~1995"`
  - A qualifier before two days now marks both days
    - `"approximately between 1 and 5 January 2019"` lost its qualifier

## Resolution

- Improved `as.Date()`, `as.POSIXct()`, `as.POSIXlt()`, and `as.double()`
  - Changed them to state the rule for imprecise values when `FUN` is not given
    - They resolve with `vmin()` as before, and say so once in each session
    - In 1.3.0 this becomes a warning, and in 1.4.0 an error
    - Precise values need no `FUN`
  - Fixed them dropping further arguments to `FUN`
    - `as.Date(x, FUN = vmin, approx_range = 3)` now widens approximate values
  - Added `FUN` to `as.double()`, and fixed it giving an error for `NA`
- Improved `random()` and `vrandom()` for approximate and uncertain values
  - Added `approx_range` to widen `~` and `%` values with trapezoidal weights
    - The weight falls linearly to 0 over the dates that `approx_range` adds
    - This follows the egg-yolk model (Cohn and Gotts 1996)
    - Each mark widens by its own component, and each range end by its marks
    - `"1990-03-~01..1990-04-~15", approx_range = 3` gives 1990-02-26 to 1990-04-18
    - Open ranges, sets, `X` digits, BCE dates, and date-times are not widened yet
  - Added `uncertain_p`, the probability that a `?` or `%` value is correct
    - One number, or one for each value, e.g. the reliability of each source
    - Otherwise `vrandom()` gives `NA` and `random()` leaves the value out
  - Updated their documentation to a help page of its own
- Added `approx_range` to the other resolution functions
  - `vmin()`, `vmax()`, `min()`, and `max()` give the ends of the widened support
    - `vmin("2001-05-16~", approx_range = 3)` gives `"2001-05-13"`
  - `mean()`, `median()`, `vmean()`, and `vmedian()` use the weights of `vrandom()`
  - `modal()` and `vmodal()` give the same result, as the core has the most weight
- Added `uncertain` to the other resolution functions
  - `min()`, `max()`, `mean()`, `median()`, `modal()`, and their `v*()` forms
  - `uncertain = "drop"` gives `NA` in `v*()` and leaves values out of the others
- Fixed `min()`, `max()`, `vmin()`, and `vmax()`
  - A set resolves to the first or last day of any member, in any order
    - `vmax("{2013-01,2012-06}")` gave `"2012-06"` and now gives `"2013-01-31"`
  - `min()` gave the wrong value when the first element was not the earliest
  - `%` stayed in the result
  - Years with unspecified digits resolved to the wrong year
    - `vmin("19XX")` gave `"1901-01-01"` and now gives `"1900-01-01"`
    - `vmax("199X")` gave the value unresolved and now gives `"1999-12-31"`
    - `vmin("XX29-01-01")` gave `"0129-01-01"` and now gives `"0029-01-01"`
    - `vmax("-00XX")` gave `"-0000-12-31"` and now gives `"0000-12-31"`
- Fixed missing values in the resolution functions
  - The `v*()` functions give `NA`
  - `min()` and `max()` follow base R: with `na.rm = FALSE`, one `NA` gives `NA`
- Fixed the `v*()` functions ignoring an argument that they do not know
  - `vmin(x, approx_range = 3)` gave the date without widening, and no warning
  - A misspelled argument, e.g. `aprox_range`, is now an error
- Accelerated `vmin()`, `vmax()`, `min()`, and `max()`
  - A resolved plain date does not go through the parser again
  - ~6x faster for `vmin()` on 60,000 ranges
- Accelerated `mean()`, `median()`, `modal()`, `random()`, and their `v*()` forms (#105)
  - A range expands without being parsed again
  - ~15x faster for `mean()` on 1,000 months and ranges
- Accelerated `vrandom()`, `vmean()`, and `vmedian()` on full dates and ranges
  - Full dates and ranges resolve directly, without `possible_dates()`
  - ~900x faster for `vrandom()` on 63,606 event dates, a quarter of them ranges
  - ~290x faster for `vmean()` on the same dates

## Expansion

- Renamed `expand()` to `possible_dates()`, gives days only
  - Use `seq()` for a sequence of times
- Removed `contract()`; use `as_messydate()` to give the shortest form of `x`
  - Fixed the shortest form of a range that does not cover whole months or years
    - `"2012-02-01..2013-01-31"` was written as `"2012-02"`
    - `"2012-01-01..2013-12-31"` was written as `"2012"`
    - `"2012-02-01..2012-02-28"` was written as `"2012-02"`; February 2012 has 29 days
  - Fixed the shortest form of a set of dates before the common era
    - `{-0050-01-01,-0050-03-01}` was written as `-0050-01-01..-0050-03-01`
- Accelerated `possible_dates()` on ranges of two full dates (#105)
  - Plain ranges and set members are expanded in one step
  - ~20x faster for 1,000 months and ranges
  - ~8x faster for 60,000 ranges
- Improved `possible_dates()` with `approx_range`
  - Fixed widening an approximate year from 1 January only
    - `"~2012"` with `approx_range = 1` ended on 2013-01-01, now 2013-12-31
    - Widening starts from the first and the last date that the value names
  - Fixed ranges with `~` that were not widened
  - Fixed open-ended ranges with `~`, which now give their known end
    - `"..2001-01-~05"` with `approx_range = 3` gave 1998-01-06 to 2001-01-05
  - Each mark widens by the component that it is written on
    - `"2019-01-01~"` widens by days and `"2019-01~"` by months
    - `"~2019-01-01"` and `"~2019"` widen by years
    - `"2019-~01-~01"` widens by months and days
  - Fixed `%` being ignored; it marks a value as approximate and uncertain
- Fixed `expand()` with a sub-day `by` dropping the UTC offset of a range (#107)
- Fixed `possible_dates()`, `vmin()`, and `vmax()` on a month or day with one unspecified digit
  - `"2012-1X"` stopped with an error, and now gives October to December 2012
  - A value with no valid day, e.g. `"2012-02-3X"`, gives no dates
- Fixed `possible_dates()` reading a year with leading `X` digits as one range
- Fixed `seq()` and `possible_dates()` on an invalid element, e.g. `""`
- Fixed missing values in `possible_dates()` to give `NA`

## Extraction

- Fixed missing values in `precision()` to give `NA`
- Added `approx_range` to `precision()` to count the widened days

## Operations

- Changed `%intersect%` and `%union%` to work element by element and give an `mdate`
  - The result is in the shortest form, e.g. `"2012-01-03..2012-01-05"`, not days
  - The two sides are recycled to a common length
  - An intersection with no days gives `{}`, and a missing value gives `NA`
  - A `?` or `~` on either side is carried to the result
- Fixed `+` and `-` with `"1 year"` or `"1 month"`, which added 365 or 30.42 days
  - `as_messydate("2012-01-01") + "1 year"` gave `"2012-12-31"`, now `"2013-01-01"`
  - Months and years shift the calendar with rollback
  - Whole months stay whole months
  - Added `"week"` as a unit, which gave an error
- Fixed `+` and `-` on dates before the common era, which moved them the wrong way
  - `as_messydate("-0050-01-01") + 1` gave `"-0049-12-31"`, now `"-0050-01-02"`
- Fixed missing values in `+` and `-`, which now give `NA`
- Accelerated `<`, `>`, `<=`, and `>=` on two `Date` or `POSIXt` values
  - Without an `mdate` on either side, they give the base R result
  - A `Date` with a `POSIXct` is still compared by its time, which base R does not do
  - >2000x faster for 100,000 `Date` comparisons while messydates is loaded
- Added `approx_range` to `%l%`, `%g%`, `%le%`, `%ge%`, `%><%`, and `%>=<%`
  - Each day of an approximate value counts by the trapezoidal weight of `vrandom()`
  - Call the operator as a function to set it, e.g. `` `%l%`(x, y, approx_range = 3) ``
- Fixed `%><%` and `%>=<%` giving 0 and not `NA` for a missing value
- Accelerated `%l%`, `%g%`, `%le%`, `%ge%`, `%><%`, and `%>=<%`
  - A date, month, year, or range is measured from its first and last day
  - ~17x faster for 60,000 pairs of ranges
- Renamed the `is_*()` predicates to `date_is_*()`
  - `is_precise()`, `is_uncertain()`, `is_approximate()`, `is_bce()`, `is_similar()`
  - `is_messydate()` keeps its name, because it is a class test
- Renamed `is_intersecting()` and `is_subset()` to `date_is_intersecting()` and `date_is_subset()`
  - They are now element-wise: they compare `x[i]` with `y[i]` and recycle as base R does
  - Missing values give `NA`
- Fixed `is_subset()` to test that all dates of `x` are in `y`, not only one date
- Fixed `is_subset()` using only the first element of `x` and `y`
- Added `date_is_open()` to test for an open range, e.g. `..2012`
- Added `approx_range` to `date_is_intersecting()` and `date_is_subset()`
  - `date_is_intersecting("2001-05-16~", "2001-05-18", approx_range = 3)` gives `TRUE`

## Plotting

- Added `plot()` for `mdate` to draw the weight of each possible day
  - Precise dates are points, and ranges are bars
  - Sets and `X` digits are separate points
  - With `approx_range`, approximate values are trapezoids, as in `vrandom()`
  - Uncertain values have a dashed outline

## Data

- Fixed `battles$Date`, which was saved before `mdate` used `vctrs`

# messydates 1.1.1

## Package

- Added a reverse dependency job to the PR checks workflow

## Coercion

- Added parsing of a day of the year, e.g. "the 103rd day of 2026" (closes #97)
  - Gives `"2026-04-13"`; `"2026-103"` notation is still rejected as ambiguous
- Added parsing of a week of the year, e.g. "the 5th week of 2026" (closes #96)
  - Gives the range `"2026-01-26..2026-02-01"`
  - Weeks follow ISO 8601, so week 1 can start in December of the year before
  - `"2026-W05"` and `"2026-W05-3"` are read too, but recorded as dates/ranges
- Updated the documentation of seasons in prose, e.g. "summer 2026" (closes #98)
  - Gives the northern-hemisphere month range `"2026-06..2026-08"`
  - `"2026-22"` season codes are still rejected as ambiguous
- Improved the errors for ordinal dates and season codes to name the prose form
- Fixed "before" and "after" keeping both bounds of a range that followed
  - "before summer 2026" gave `"..2026-06..2026-08"` and now gives `"..2026-06"`
- Fixed prose naming an impossible date becoming `NA` without a warning
  - "the 53rd week of 2025" now warns, as other unparseable text does

# messydates 1.1.0

## Package

- Improved package architecture documentation in `.github/CONTRIBUTING.md` so 
  that human contributors and coding agents read the same notes
- Added a PR metadata job to the PR checks workflow, which verifies that the
  `DESCRIPTION` version is bumped, that the PR title names the new version, and
  that the PR description itemizes changes under subsection titles; these items
  are consequently dropped from the pull request template
- Fixed the website deploy job installing `Config/Needs/check` packages instead
  of `Config/Needs/website`

## Class

- Improved `validate_messydate()` to report which elements failed and what they
  contained, instead of naming only the rule that was broken
  - No longer passes a vector of empty strings as valid because a single element 
    somewhere in it happened to contain a digit
- Assigning an unparseable value into an `mdate` with `[<-` or `[[<-` now
  reports what could not be parsed, rather than silently blanking the element
- Added `unique()` and `duplicated()` methods for `mdate` objects (closes #106)
  - Previously `unique()` fell through to the character method and dropped the
    class, so a deduplicated column silently stopped being an `mdate`
  - Both compare the annotated strings rather than the dates they expand to,
    so `"2012-01"` and `"2012-01-01..2012-01-31"` remain distinct

## Coercion

- Improved `as_messydate()` to validate its input rather than carrying values it
  cannot interpret:
  - dates with impossible components are rejected, so `"2019-02-30"`,
    `"2019-06-31"` and `"2019-01-01 25:00"` now error instead of being accepted
    as written; relatedly, `"2019-13-45"` is no longer silently reordered into
    `"2019-45-13"`, since month and day are only swapped where the swap yields
    a date that could exist
  - ISO 8601-2 notations that `{messydates}` does not represent (week dates,
    ordinal dates, season codes, significant digits, extended years, the
    `P`-style duration notation, and repeating intervals) now error naming the
    format, instead of passing through as strings that nothing downstream could
    expand, resolve or compare. Durations themselves are unaffected: they are
    written as date ranges and handled by `mduration`, and the error for
    `"P1Y2M"` points there
  - text naming no date at all still becomes `NA`, but now warns, listing what
    could not be read
- Added `md_problems()`, which reports one row per unparseable element of a
  vector, with the reason for each, for checking a column of dates before
  coercing it
- Added `as_messydate()` methods for factors (coerced via their labels, the
  common case when a column was read in with `stringsAsFactors = TRUE`) and a
  default method that names the offending class rather than failing with R's
  `UseMethod` message
- Improved parsing consistency across scalars and vectors
  - a month-first date such as `"July 4 1976"` previously lost its day whenever 
    another value shared the vector with it
- Fixed how `[]` sets ("one member of") silently rewrote as `{}` sets ("all members of"). 
  - Note that the two still expand and resolve alike; giving
    `[]` its own meaning in the resolution functions remains outstanding
- Fixed BCE/CE prose off-by-one error (closed #94, thanks @njbart) 
  to convert to ISO 8601-2 astronomical year numbering 
  (proleptic Gregorian, in which a year zero exists and equals 1 BCE):
  - A historical `N BCE` maps to the astronomical year `-(N-1)`, so `"44 BC"`
    becomes `-0043` and `"1 BC"` becomes `0000` (year zero); a signed ISO year
    such as `"-0044"` is already astronomical and is left unchanged
  - Year zero is preserved on input (previously `0000` was misread as an
    unspecified year) and is traversed by `seq()` and `expand()`, so a sequence
    spanning the BCE/CE boundary now passes through the whole of year `0000`
    rather than jumping from `-0001` straight to `0001`
  - Era markers *on input* are now resolved for each year in an expression
    separately, rather than by counting how many markers a string contains
    (they are, as before, always removed in the parsed `mdate`, which records
    the era in the sign of the year alone). So an input marker written once at
    the end of a range or set applies to every bound of it: `"..200 BC"`,
    `"200..100 BC"` and `"44, 33 BC"` now give `..-199`, `-0199..-0099` and
    `{-0043,-0032}`, where previously the non-leading bounds silently stayed
    CE; an input marker written before a date still governs that date, so
    `"{BC2010-10-10,BC2010-10-11}"` gives `{-2009-10-10,-2009-10-11}`; and
    `"200 BC..100 AD"` spans the two eras, giving `-0199..0100`
  - Fixed how era markers could be dropped from approximate or uncertain dates
    given in prose, so `"circa 200 BC"` is now `~-199` rather than `0200~`

## Expand/Contract

- Fixed how `expand()` handles unspecified years
  - Previously `"192X"` raised `'from' must be a finite number` and 
    `"18XX"` silently returned a single date, 
    even though these are what the prose parser produces for decades and centuries
  - Now a bare unspecified year expands to the whole span (`"192X"` gives every
    day of the 1920s)
  - Now an attached month or day picks out that month or day in each candidate 
    year (`"192X-05-04"` gives ten dates, not nine years of them)
  - Now BCE years are bounded the other way round, `-1999` being earlier
    than `-1900`
  - Now a year too vague to enumerate, such as `"XXXX"`, refused with a message
    suggesting `vmin()`/`vmax()`
- Fixed how `expand()` applies unspecified-component rules to each member of a set
  separately, fixing over-expansion of sets whose members had an unspecified
  month: `"{2008-XX-31,2009-XX-31}"` gave 671 dates and now gives 24
- Fixed `contract()` returning every set in `{}` notation, so that a `[]` set
  no longer became a `{}` set on a round-trip through `expand()`/`contract()`
  (closes #99)
  - Only applies where `contract()` is given an `mdate`, since a list of dates
    does not record which kind of set its members came from
  - Documented what the two set types mean once resolved or operated on, having
    established that both should continue to expand to the same members (closes #99):
    - `?resolve_tendency` now distinguishes a central tendency that describes
      where several recorded occurrences sit (`{}`) from one that is a point
      estimate of a single unknown date (`[]`), as for a range
    - `?operate_set` and `?operate_proportional` now note that a result for a
      `[]` set reads as the candidates that remain possible, or the probability
      that a comparison holds, rather than a share of recorded occurrences

# messydates 1.0.0

## Package

- Updated the `{messydates}` logo to better reflect the package's purpose
  and be more brand-consistent with manydata packages
- Removed `purrr` and `dplyr` dependencies (replaced with base R), 
  leaving only `stringi` and `lubridate` as imports
- Updated the cheatsheet to reflect the new time support and other changes
- Declared `anytime` and `clock` in Suggests, since the interoperability
  tests exercise them where installed

## Class

- Added support for ISO 8601-2:2019 **times** of day in the `mdate` class
  - `as_messydate()` now parses an optional date-time separator, 
    `hh`, `hh:mm`, and `hh:mm:ss` (with fractional seconds), am/pm times, the UTC designator
    `Z`, and numeric offsets (e.g. `+02:00`), zero-padding and normalising them
  - Dates and times are separated by a space (e.g. `2019-03-01 14:30:00`)
    for readability (as permitted by ISO 8601-1 sec. 4.3.2 and RFC 3339),
    but a `T` separator can be used on input and is normalised to a space in the output
  - `:` and `_` continue to work as range separators; 
    times are detected and protected first, 
    so `2009-01-01:2019-01-01` is a range while `2019-03-01 14:30:00` is a time
  - Time components can carry the same annotations as dates: 
    approximate (`~`), uncertain (`?`), both (`%`), and unspecified (`X`),
    e.g. `2019-03-01 ~14:30`
  - A time of day can now be given on its own, with no date part 
    (e.g. `as_messydate("2:30pm")` -> `14:30`, `as_messydate("around 2pm")` -> `14:00~`); 
    this requires a clear time signal (a colon-clock or am/pm), so a bare `2019` 
    is still a year, and a bare am/pm hour (`2pm`) fills to `14:00`. 
    A leading "at" (`"at 2:30pm"`, `"at around 2pm"`) is recognised and dropped. 
    `hour()`/`minute()`/`second()`/`tz()`, `precision()`, `expand()`, 
    and `approximate()`/`uncertain()` all handle date-less times
- Renamed `messyduration()` to `make_messyduration()` for consistency with other `make_*()` functions, 
  and renamed resulting class from `messyduration` to `mduration`
- Fixed bug where a messyrange would shift instead of widen upon approximation

## Coercion

- Improved parsing of written dates (closed #52): 
  - Ordinal days (`"4th July 1976"`): either day-first or month-first order 
    (`"Fourth of July 1976"`, `"July 4th 1976"`), "day of" phrasings, 
    and "last day of `<month>`" (leap-year aware for February)
  - Connectives: 
    - `"between the 13th and 15th of Feb 1977"` (or `"from the 13th to the 15th"`) becomes a range
    - `"the 13th or the 15th"` a set
    - plain `"13th and 15th"`, or a comma-separated list of dates, becomes several dates
  - Reduced-precision expressions: 
    - month-and-year (`"February 2004"` -> `2004-02`)
    - decades (`"the 1910s"` -> `191X`)
    - centuries (`"the 19th century"` -> `18XX`)
  - Open ranges: `"before 1910"` -> `..1910` and `"after 1910"` -> `1910..`, 
    where the bound may itself be imprecise (`"before the 1920s"` -> `..192X`)
  - Prose qualifiers: 
    - approximate words (`"around"`, `"circa"`, ...) add `~`
    - uncertain words (`"possibly"`, `"reportedly"`, ...) add `?`
    - both together add `%`, applied to the most specific component
  - Roman numerals (e.g. `"MDCCLXXVI"` becomes `1776`)
  - Roman calendar references, e.g. `"the Ides of March, 44 BC"` becomes `-0044-03-15` 
  (Kalends, Nones, and Ides, with the later Nones/Ides of March, May, July, and October)
- Coercion from `POSIXct`/`POSIXlt` now preserves the time of day (midnight is
  treated as date-only); `as.POSIXct()`/`as.POSIXlt()` restore it
- Improved interoperability with `{lubridate}`: its `as_date()` and 
  `as_datetime()` coercion verbs now work on an `mdate`, honouring the `FUN` 
  resolver (e.g. `as_date(md, FUN = vmax)`), as do `format()` on `mdate` columns 
  in data frames and tibbles
- Fixed `as.POSIXct()`/`as.POSIXlt()` erroring on a vector of two or more 
  `mdate`s (the check for pre-common-era dates was not vectorised)
  
## Annotation

- Renamed `as_approximate()`/`as_uncertain()` to `approximate()`/`uncertain()` 
  for consistency with `on_or_before()`/`on_or_after()` (these aren't coercion functions); 
  old names are defunct and warn
- Improved `approximate()`/`uncertain()` to accept 
  "hour", "minute", "second", and "time" components
- Improved `approximate()`/`uncertain()` to combine annotations as `%` 
  when both are applied to the same component

## Resolution

- Fixed how `median()`/`vmedian()` treat an even number of dates,
  which no longer silently returns `NA` but instead averages the two middle values
- Fixed how `mean()`/`vmean()`/`median()`/`vmedian()` average precise date-times,
  honouring the time of day, instead of miscomputing via `lubridate::as_date()`
  
## Operations

- Improved arithmetic (`+`/`-`) and `seq()` to accept sub-day units 
  ("hours", "minutes", "seconds") and operate on times
  - `messyduration()` keeps sub-day precision
- Improved how arithmetic treats time of day and shifts the calendar components, 
  so `"2012-02-03 14:30" + "1 year"` is `2013-02-03 14:30` (with month-end rollback)
- Fixed bug where adding/subtracting from an open-ended range (`"2012-01-01T09:00.." + "2 hours"` 
  or `"..2012-01-01T09:00" + "1 month"`) dropped `..` marker
- Fixed how `<`/`>`/`<=`/`>=` compare time of day on the same calendar day, 
  no longer silently truncating both sides to a date first and treating them as equal
- Fixed two related bugs (#92) affecting *any* comparison of two `Date`/`POSIXct` 
  objects in a session with `{messydates}` loaded, including in unrelated packages:
  - since `<`/`>`/`<=`/`>=` are registered for `"Date"`/`"POSIXt"` classes
    (so that e.g. `Date < mdate` works), 
    *any* comparison of a zero-length or all-`NA` `Date`/`POSIXct`/`POSIXlt` value
    was passed through `as_messydate()`, 
    where `ifelse()` and `paste0()` silently changed type or length
    and tripped an internal `is.character()` check. 
    This broke loading packages whose `.onLoad` hooks compare timestamps, 
    such as `{httr2}`'s cache pruning, which in turn broke `pkgdown::build_news()`
  - when one side of a comparison had a time of day and the other did not, 
    their numeric bounds were computed in different units (e.g. seconds, days)
    without converting to a common unit, which could silently reverse the result. 
    In particular, `Sys.time() < (Sys.time() + Inf)` -- 
    the pattern `{httr2}` uses to represent an unbounded retry deadline -- 
    incorrectly evaluated to `FALSE`, in turn breaking `httr2::req_perform()` 
    (and so any request made while `{messydates}` is loaded, 
    including `pkgdown`'s GitHub release-timeline lookup)

## Extraction
  
- Added `hour()`, `minute()`, `second()`, and `tz()` time component extractors
- Improved `month()`, `day()`, and `precision()` to be vectorised and 
  no longer call `expand()` more than necessary
- Improved `precision()` to extend below the day: 24 to the hour, 1440 to the
  minute, and 86400 to the second (date-level precision is unchanged)
- `year()`, `month()`, `day()`, `hour()`, `minute()`, `second()`, and `tz()` are 
  now S3 methods on the same-named `{lubridate}` generics rather than plain 
  functions, so the two packages can be loaded together (in either order) 
  without one masking the other: these accessors dispatch to the messy-date 
  logic on an `mdate` but to `{lubridate}`'s own methods on a `Date`/`POSIXct`

## Expand/Contract

- Fixed `expand()` error when `approx_range` was set and the vector contained a 
  reduced-precision value (e.g. a bare year-month) alongside an approximate one
- Improved `expand()` with a `by` argument (default `"day"`)
  - Ranges are enumerated at day granularity to avoid combinatorial explosion;
    set `by` to `"hour"`, `"min"`, or `"sec"` for finer enumeration 
  - Precise date-times keep their time
  
# messydates 0.5.4

## Coercion

- Improved how `as_messydates()` handles text with dates in American format,
e.g. October 10, 2010 (fixes #86)

# messydates 0.5.3

## Components

- Improved `year()` to be faster and work on durations
- Improved `precision()`
  - `precision()` is now a S3 generic, dispatching to `precision.mdate()`
  - `precision.mdate()` now returns the inverse of the previous measure,
  meaning maximising precision makes more sense

# messydates 0.5.2

## Package

- Moved `mreport()` to `{manydata}`
- Consolidated and renamed scripts internally

## Coerce to

- Fixed pkgdown#2855 by fixing how as_messydate methods interpret infinite dates
- Fixed time zone defaults in `as.POSIXct.mdate()` and `as.POSIXlt.mdate()`
- Fixed set bug in `validate_messydate()`

## Coerce from

- Renamed `as.numeric()` to `as.double()` to fix S3 dispatching
- Separated extrema functions into `min.mdate()` and `max.mdate()` for summaries 
and `vmin.mdate()` and `vmax.mdate()` for vector coercion
- Separated tendency functions into `mean.mdate()`, `median.mdate()`, and `modal.mdate()` for summaries 
and `vmean.mdate()`, `vmedian.mdate()`, and `vmodal.mdate()` for vector coercion
- Vector coercion previously in `random.mdate()` now in `vrandom.mdate()`
- Improved how coercion/resolution functions handle BCE dates

## Manipulation

- Fixed how `precision()` calculates precision

# messydates 0.5.1

## Package

- Fixed redirected url in README
- Fixed pkgdown help links

# messydates 0.5.0

## Package

- Dropped the vignettes as they were 'outdated'
- Dropped a number of tests to provide rapid testing framework
- Dropped `{tibble}` dependency by just using `{dplyr}`
- Updated Github workflows
- Updated testthat to version 3, tests now run in parallel
- Updated pkgdown to bootstrap 5
- Updated DESCRIPTION with config packages

## Functions

- Moved from `{stringr}` to `{stringi}` for _speed_
- Added `as.numeric.mdate()` and `as_messydate.numeric()` for coercing between messydates and numbers (closes #85)
- Added `seq.mdate()` for creating sequences from one or two messydates
  - This includes correct sequences for leap years and historical dates including before the common era
- Added `is_bce()` for testing whether dates are from before the common era
- Added `stri_squish()` helper for trimming white space everywhere
- Improved `c.mdate()` so that it will strip class from an `mdate` object, as expected
- Improved `is_uncertain()` and `is_approximate()` so that they also recognise `%` annotations
- Improved `min.mdate()`, `max.mdate()`, and `modal.mdate()` to avoid using `expand()` and consequently run much faster
- Improved `min.mdate()`, `max.mdate()`, `modal.mdate()`, `mean.mdate()`, `median.mdate()`, and `random.mdate()` by adding `recursive` argument for resolving vectors down to a scalar
- Fixed bug in `messyduration.mdate()` where the minimum of an underspecified later date was used
- Fixed bug in `as_messydate()` where zero padding for early dates was not added correctly

# messydates 0.4.1

## Package

* The package now depends on R versions bigger or equal to 4.0 since functions for subsetting and comparing 'mdate' objects rely on functions introduced in that version

## Functions

* Closed #83 by fixing how logical comparisons works for negative and year only dates

# messydates 0.4.0

## Functions

* Closed #46 by adding the `mdates_duration` class that introduces methods to annotate a duration or period with representations of its uncertainty
* Closed #72 by fixing issues with double unspecified components not being contracted correctly
* Closed #73 by fixing bugs with the conversion of dates where month is spelled
* Closed #74 and #82 by adding other logical comparison operators for 'mdate' objects (e.g. `<`, `>`, `<=`, `>=`) (thanks @WerthPADOH)
* Closed #76 by adding proportional operators that calculate the proportion of messy dates meeting logical tests (e.g. `%l%` `%le%`, `%g%`, `%ge%`, `%><%`, `%>=<%`)
* Closed #77 by adding basic vector methods for subsetting and data frames (thanks @WerthPADOH)
* Added alias function `mdate()` for `as_messydate()`
* Renamed set family of functions to work as operators (i.e. `%intersect%` and `%union%`)
* Replaced `is_element()` by `is_subset()` for clarity and consistency
* Closed #80 by updating `make_messydates()` function to also construct ranges of dates

# messydates 0.3.5

## Functions

* Updated how `contract()` function checks if 'mdate' object has been expanded 

# messydates 0.3.4

## Package

* Updated 'battles' internal data
  * Corrected issues with zero padding for certain date ranges
  * Added 'US_party' and 'N_actors' additional variables for replication purposes

## Functions

* Closed #68 by updating `as_messydate()` function
  * Fixed bugs with zero padding for ranges of dates
  * Fixed bugs with the re-ordering of months and day components for incorrectly specified dates
* Closed #69 by updating `contract()` function to 'expand' dates before 'contracting' them
* Updated `expand()` function to handle, and properly convert, date objects that are not 'mdate'

# messydates 0.3.3

## Package

* Moved cheatsheet.pdf to 'inst' folder instead of the 'man' folder

# messydates 0.3.2

## Package

* Closed #64 by updating failing tests to test for other aspects instead of the printing of negative dates across OS
* Closed #65 by updating cheatsheet for new package changes

## Functions

* Closed #62 by adding "resequence" as an argument to `as_messydates()` for explicit date format conversion, if necessary
* Closed #63 by fixing issues with unnecessary white spaces added in date conversion

# messydates 0.3.1

## Package

* Updated README by removing unattractive package startup messages
* Updated `battles` data by adding 'parties' variable

## Functions

* Closed #54 by adding new `mreport()` function to properly report on data containing 'mdate' variables
* Updated `expand()` function
  * Fixed bug with the expansion of approximate dates
  * Removed unnecessary function message

# messydates 0.3.0

## Package

-   Closed #51 by changing object class name to `mdate`
    -   Note that this is a *breaking* change
-   Closed #41 by creating `{skimr}` template for `mdate` class

## Functions

-   Updated coercion to messy dates
    -   Closed #26 by adding "resequence" argument to `as_messydate()` allowing users to choose component order of ambiguous dates
    -   Closed #45 by improving how `as_messydate()` re-orders 6 digit date components if necessary
    -   Closed #48 by adding zero padding incomplete date ranges and sets of dates
    -   Updated `as_messydate()` to also extract dates from text strings
-   Added `is_precise()` function that provides a logical test for precise dates
-   Updated messy dates expansion
    -   Updated `expand()` to allow for the expansion of incomplete date ranges and sets of dates
    -   Closed #49 by updating resolve functions to only expand dates if they are not precise

# messydates 0.2.1

## Package

-   Added a vignette for working with the `{messydates}` package

## Functions

-   Closed #9 by adding arithmetic operations for working with `messydt` objects
    -   Added S3 methods for "+" and "-" operators
    -   Added `add()` and `subtract()` helper functions for arithmetic operations
-   Updated `expand()` function
    -   Closed #31 by updating how approximate dates are expanded to account for leap years
    -   Closed #34 by updating `expand()` to manage negative dates
    -   Added `expand_negative_dates()` helper function for expanding ranges of negative dates
-   Updated functions that coerce from `messydt` objects to `Date` to manage negative dates
    -   Added `negative_dates()` helper function to coerce negative `messydt` dates
-   Closed #39 by updating how resolve mean methods work for negative dates
-   Closed #40 by updating contract function to manage the contraction of negative dates
    -   Added `compact_negative_dates()` helper function to compact negative date ranges
    -   Added `is.sequence()` as a helper function to check if dates are a range
-   Updated resequence script to export `interleave()` function

# messydates 0.2.0

## Package

-   Added PANARCHIC project details to README file
-   Added cheatsheet
-   Added a new CSS style to website and updated functions displayed
-   Addressed workflow actions issues
    -   Updated pushrelease.yml workflow actions file to stop installing `{messydates}` from Github
    -   Updated README file to stop installing `{messydates}` from Github
-   Fixed Codecov test coverage URL on README file for CRAN submission

## Functions

-   Expanded on messydates checks for class validity
-   Fixed bugs for `make_messydate()`
-   Added annotation functions and standardized annotation so that it is consistent with ISO2019E standards.
    -   `on_or_before()`
    -   `on_or_after()`
    -   `as_approximate()`
    -   `as_uncertain()` (includes discrimination between month uncertainty and day and month uncertainty)
-   Updated `as_messydate()` by adding zero padding for month, day or year
-   Updated `resequence()` to work consistently with messydate objects
-   Updated `expand()` function to expand imprecise, unspecified, approximate, uncertain, and negative dates according to approximate ranges and added tests
-   Updated `precision()` to return the lengths of expanded dates
-   Updated `median()` in resolve family of functions to work with changes to `expand()`
-   Added tests for functions
    -   Added tests for `expand()`
    -   Added tests for `contract()`
    -   Added tests for `precision()`
    -   Added tests for `coerce_from_messydate()`

# messydates 0.1.1

## Package

-   Updated README with some more explanation about what the package does/offers
-   Fixed URL to the package website

# messydates 0.1.0

## Package

-   Updated call to `messydt` class in DESCRIPTION file

## Functions

-   Updated documentation for `as_messydate()` functions
-   Updated documentation for `expand()` function
-   Updated documentation for resolve family of functions
-   Updated documentation for coerce from family of functions
-   Updated documentation for coerce to family of functions

# messydates 0.0.1

## Package

-   Setup `{messydates}` package
    -   Added `DESCRIPTION` file
    -   Added `R` folder
    -   Added `LICENSE` file
    -   Added `NAMESPACE` file
    -   Added `NEWS` file
    -   Added `README` files
    -   Added `.github` folder and files
    -   Added `tests` folder and files
-   Setup pkgdown website
-   Added package logo

## Functions

-   Added a new `messydt` class which follows the latest ISO 8601 (2019) standards
    -   Added validation checks for messydt class
    -   Added print methods for messydt class
-   Added `as_messydate()` function to coerce from date objects to messydate
    -   `as_messydate()` standardises date order, separators and ambiguity
    -   Added date class coercion
    -   Added POSIXct class coercion
    -   Added POSIXlt class coercion
    -   Added character class coercion
-   Added functions to coerce from messydate objects to other date classes
    -   Added `as.Date.messydt()` for coercing to date class
    -   Added `as.POSIXct.messydt()` for coercing to POSIXct class
    -   Added `as.POSIXlt.messydt()` for coercing to POSIXlt class
-   Added `expand()` function for expanding ranged and uncertain dates
-   Added functions to resolve expanded dates
    -   Added `min.messydt()` to get minimum value from expanded range
    -   Added `max.messydt()` to get maximum value from expanded range
    -   Added `median.messydt()` to get median value from expanded range
    -   Added `mean.messydt()` to get mean value from expanded range
    -   Added `modal.messydt()` to get mode value from expanded range
-   Added `contract()` function for contracting expanded dates
-   Added extract functions to get particular date components
    -   Added `year()` to extract year from date
    -   Added `month()` to extract month from date
    -   Added `day()` to extract day from date
-   Added `make_messydate()` function to get messy dates from multiple columns
-   Added set functions for operations in sets of messy dates
    -   Added `md_intersect()` to find intersection of sets of messy dates
    -   Added `md_union()` to find union of sets of messy dates
    -   Added `md_multiset()` to join two sets of messy dates
-   Added logical function for various logical tests for messy date objects
    -   Added `is_messydate()` to test for messydt class
    -   Added `is_intersecting()` to test if dates intersect
    -   Added `is_element()` to test for multiple elements in dates
    -   Added `is_similar()` to test for similarities in dates
-   Added tests for new functions
    -   Added tests for messydt class and `às_messydate()` function
    -   Added tests for coerce from messy dates functions
    -   Added tests for coerce to messy dates functions
    -   Added tests for `contract()` function
    -   Added tests for `expand()` function
    -   Added tests for extract functions
    -   Added tests for `make_messydate()` function
    -   Added tests for resolve functions
    -   Added tests for set functions

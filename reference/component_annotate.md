# Annotates dates as censored, uncertain, or approximate

Some datasets have for example an arbitrary cut off point for start and
end points, but these are often coded as precise dates when they are not
necessarily the real start or end dates. This collection of functions
helps annotate uncertainty and approximation to dates according to
ISO2019E standards. Inaccurate start or end dates can be represented by
an affix indicating "on or before", if used as a prefix (e.g.
`..1816-01-01`), or indicating "on or after", if used as a suffix (e.g.
`2016-12-31..`). Approximate dates are indicated by adding a tilde to
year, month, or day components, as well as groups of components or whole
dates to estimate values that are possibly correct (e.g. `2003-03-03~`).
Day, month, or year, uncertainty can be indicated by adding a question
mark to a possibly dubious date (e.g. `1916-10-10?`) or date component
(e.g. `1916-?10-10`).

## Usage

``` r
on_or_before(x)

on_or_after(x)

approximate(x, component = NULL)

uncertain(x, component = NULL)
```

## Arguments

- x:

  A date vector

- component:

  Annotation can be added on specific date components ("year", "month"
  or "day"), or to groups of date components (month and day ("md"), or
  year and month ("ym")). This must be specified. If unspecified,
  annotation will be added after the date (e.g. `1916-10-10?`),
  indicating the whole date is uncertain or approximate. For specific
  date components, uncertainty or approximation is annotated to the left
  of the date component. E.g. for "day": `1916-10-?10` or `1916-10-~10`.
  For groups of date components, uncertainty or approximation is
  annotated to the right of the group ("ym") or to both components
  ("md"). E.g. for "ym": `1916-10~-10`; for "md": `1916-?10-?10`.

## Value

A `mdate` object with annotated date(s)

## Details

For date-times, `component` may also be "hour", "minute", or "second"
(the marker is placed to the left of that time component), or "time"
(the whole time of day is marked).

## Functions

- `on_or_before()`: prefixes dates with ".." where start date is
  uncertain

- `on_or_after()`: suffixes dates with ".." where end date is uncertain

- `approximate()`: adds tildes to indicate approximate dates/date
  components

- `uncertain()`: adds question marks to indicate dubious dates/date
  components.

## Examples

``` r
data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
  End = c("1816-12-31", "1916-12-31", "2016-12-31"))
transform(data, Beg = ifelse(Beg <= "1816-01-01",
  as.character(on_or_before(Beg)), Beg))
#>            Beg        End
#> 1 ..1816-01-01 1816-12-31
#> 2   1916-01-01 1916-12-31
#> 3   2016-01-01 2016-12-31
transform(data, End = ifelse(End >= "2016-01-01",
  as.character(on_or_after(End)), End))
#>          Beg          End
#> 1 1816-01-01   1816-12-31
#> 2 1916-01-01   1916-12-31
#> 3 2016-01-01 2016-12-31..
transform(data, Beg = ifelse(Beg == "1916-01-01",
  as.character(approximate(Beg)), Beg))
#>           Beg        End
#> 1  1816-01-01 1816-12-31
#> 2 1916-01-01~ 1916-12-31
#> 3  2016-01-01 2016-12-31
transform(data, End = ifelse(End == "1916-12-31",
  as.character(uncertain(End)), End))
#>          Beg         End
#> 1 1816-01-01  1816-12-31
#> 2 1916-01-01 1916-12-31?
#> 3 2016-01-01  2016-12-31
# time components can be annotated too
approximate("2019-03-01 14:30:00", "hour")
#>  'mdate' chr "2019-03-01 ~14:30:00"
uncertain("2019-03-01 14:30:00", "second")
#>  'mdate' chr "2019-03-01 14:30:?00"
```

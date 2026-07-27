# Logical statements on messy dates

These functions provide various logical statements about messy date
objects.

## Usage

``` r
is_messydate(x)

is_intersecting(x, y)

is_subset(x, y)

is_similar(x, y)

is_precise(x)

is_uncertain(x)

is_approximate(x)

is_bce(x)
```

## Arguments

- x, y:

  `mdate` or other class objects

## Value

A logical vector the same length as the `mdate` passed.

## Functions

- `is_messydate()`: tests whether the object inherits the `mdate` class.
  If more rigorous validation is required, see
  [`validate_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md).

- `is_intersecting()`: tests whether there is any intersection between
  two messy dates, leveraging
  [`intersect()`](https://rdrr.io/r/base/sets.html).

- `is_subset()`: tests whether one or more messy date can be found
  within a messy date range or set.

- `is_similar()`: tests whether two dates contain similar components.
  This can be useful for identifying dates that may be typos of one
  another.

- `is_precise()`: tests whether a date (or date-time) is precise, i.e. a
  full "yyyy-mm-dd", optionally with a time of day (down to the hour,
  minute, or second) and time zone. Non-precise dates contain markers
  that they are approximate (i.e. ~), unreliable (i.e. ?), are
  incomplete (e.g. year or year-month only, or a date with an
  unspecified `X` component), or are date ranges and sets.

- `is_uncertain()`: tests whether a date is uncertain (i.e. contains ?).

- `is_approximate()`: tests whether a date is approximate (i.e. contains
  ~).

- `is_bce()`: tests whether one or more messy dates are found before the
  common era, i.e. carry a negative (astronomical) year. Note that
  astronomical year zero (`"0000"`, historically 1 BCE) is not
  sign-negative and so returns `FALSE`; it is handled natively as a
  non-negative year throughout, which R's proleptic Gregorian `Date`
  supports directly.

## Examples

``` r
is_messydate(as_messydate("2012-01-01"))
#> [1] TRUE
is_messydate(as.Date("2012-01-01"))
#> [1] FALSE
is_intersecting(as_messydate("2012-01"),
as_messydate("2012-01-01..2012-02-22"))
#> [1] TRUE
is_intersecting(as_messydate("2012-01"),
as_messydate("2012-02-01..2012-02-22"))
#> [1] FALSE
is_subset(as_messydate("2012-01-01"), as_messydate("2012-01"))
#> [1] TRUE
is_subset(as_messydate("2012-01-01..2012-01-03"), as_messydate("2012-01"))
#> [1] TRUE
is_subset(as_messydate("2012-01-01"), as_messydate("2012-02"))
#> [1] FALSE
is_similar(as_messydate("2012-06-02"), as_messydate("2012-02-06"))
#> [1] TRUE
is_similar(as_messydate("2012-06-22"), as_messydate("2012-02-06"))
#> [1] FALSE
is_precise(as_messydate(c("2012-06-02", "2012-06")))
#> [1]  TRUE FALSE
is_precise(as_messydate(c("2012-06-02 14:30:00", "2012-06-02 ~14")))
#> [1]  TRUE FALSE
is_uncertain(as_messydate(c("2012-06-02", "2012-06-02?")))
#> [1] FALSE  TRUE
is_approximate(as_messydate(c("2012-06-02~", "2012-06-02")))
#> [1]  TRUE FALSE
is_bce(as_messydate(c("2012-06-02", "-2012-06-02")))
#> [1] FALSE  TRUE
```

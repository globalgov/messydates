# Logical operations on messy dates

These operators (`<`, `>`, `<=`, `>=`) compare `mdate` objects with each
other, or with `Date`/`POSIXct`/`POSIXlt` objects, by comparing the
range of dates each side could represent (its minimum and maximum),
rather than requiring a single, precise value on both sides. A
comparison returns `NA` wherever the two ranges overlap and the order
cannot be determined; see the examples below. For a measure of *how
much* of one side precedes or follows the other, rather than a strict
`TRUE`/`FALSE`/`NA`, see
[`?operate_proportional`](https://globalgov.github.io/messydates/reference/operate_proportional.md).

## Usage

``` r
# S3 method for class 'mdate'
e1 < e2

# S3 method for class 'mdate'
e1 > e2

# S3 method for class 'mdate'
e1 <= e2

# S3 method for class 'mdate'
e1 >= e2
```

## Arguments

- e1, e2:

  `mdate` or other class objects

## Value

A logical vector the same length as the longer of `e1` and `e2`.

## Functions

- ` < `: tests whether the dates in the first vector precede the dates
  in the second vector. Returns `NA` when the date order can't be
  determined.

- ` > `: tests whether the dates in the first vector succeed the dates
  in the second vector. Returns `NA` when the date order can't be
  determined.

- ` <= `: tests whether the dates in the first vector are equal to or
  precede the dates in the second vector. Returns `NA` when the date
  order can't be determined.

- ` >= `: tests whether the dates in the first vector are equal to or
  succeed the dates in the second vector. Returns `NA` when the date
  order can't be determined.

## Examples

``` r
as_messydate("2012-06-02") > as.Date("2012-06-01") # TRUE
#> [1] TRUE
# 2012-06-XX could mean 2012-06-03, so unknown if it comes before 2012-06-02
as_messydate("2012-06-XX") < as.Date("2012-06-02") # NA
#> [1] NA
# But 2012-06-XX cannot be before 2012-06-01
as_messydate("2012-06-XX") >= as.Date("2012-06-01") # TRUE
#> [1] TRUE
# times of day are compared for two dates on the same day
as_messydate("2012-06-02 09:00") < as_messydate("2012-06-02 17:00") # TRUE
#> [1] TRUE
```

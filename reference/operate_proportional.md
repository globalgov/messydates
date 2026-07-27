# Proportion of messy dates meeting logical test

These functions provide various proportional tests for messy date
objects, complementing the strict logical comparisons in
[`?operate_inequalities`](https://globalgov.github.io/messydates/reference/operate_inequalities.md).
Where a plain `<`/`>`/etc. comparison can only return `TRUE`, `FALSE`,
or `NA` for a messy (imprecise) date, these functions instead report
*what proportion* of the dates implied by `e1` satisfy the comparison
against `e2`, by expanding both to their full sets of possible dates
first.

## Usage

``` r
e1 %l% e2

# S3 method for class 'mdate'
e1 %l% e2

e1 %g% e2

# S3 method for class 'mdate'
e1 %g% e2

e1 %ge% e2

# S3 method for class 'mdate'
e1 %ge% e2

e1 %le% e2

# S3 method for class 'mdate'
e1 %le% e2

e1 %><% e2

# S3 method for class 'mdate'
e1 %><% e2

e1 %>=<% e2

# S3 method for class 'mdate'
e1 %>=<% e2
```

## Arguments

- e1, e2:

  `mdate` or other class objects; must be of equal length.

## Value

A numeric vector, the same length as `e1` and `e2`, of proportions
between 0 and 1.

## Details

Both kinds of set are expanded to their members, so
[`{}`](https://rdrr.io/r/base/Paren.html) ("all members of") and `[]`
("one member of") sets give the same proportion. For a `[]` set that
proportion reads as a probability that the comparison holds, under the
assumption that each candidate is equally likely, in the same way as for
a range or an unspecified component. For a
[`{}`](https://rdrr.io/r/base/Paren.html) set it instead reads as the
share of the recorded occurrences that satisfy it.

## Functions

- ` %l% `: Tests proportion of dates in the first vector that precede
  the minimum in the second vector.

- ` %g% `: Tests proportion of dates in the first vector that follow the
  maximum in the second vector.

- ` %ge% `: Tests proportion of dates in the first vector that follow or
  are equal to the maximum in the second vector.

- ` %le% `: Tests proportion of dates in the first vector that precede
  or are equal to the minimum in the second vector.

- ` %><% `: Tests proportion of dates in the first vector that are
  between the minimum and maximum dates in the second vector.

- ` %>=<% `: Tests proportion of dates in the first vector that are
  between the minimum and maximum dates in the second vector, inclusive.

## Examples

``` r
  as_messydate("2012-06") < as.Date("2012-06-02")
#> [1] NA
  as_messydate("2012-06") %l% as_messydate("2012-06-02")
#> [1] 0.03333333
  as_messydate("2012-06") > as.Date("2012-06-02")
#> [1] NA
  as_messydate("2012-06") %g% as_messydate("2012-06-02")
#> [1] 0.9333333
  as_messydate("2012-06") >= as.Date("2012-06-02")
#> [1] NA
  as_messydate("2012-06") %ge% as_messydate("2012-06-02")
#> [1] 0.9666667
  as_messydate("2012-06") <= as.Date("2012-06-02")
#> [1] NA
  as_messydate("2012-06") %le% "2012-06-02"
#> [1] 0.06666667
  as_messydate("2012-06") %><% as_messydate("2012-06-15..2012-07-15")
#> [1] 0.516129
  as_messydate("2012-06") %>=<% as_messydate("2012-06-15..2012-07-15")
#> [1] 0.5333333
```

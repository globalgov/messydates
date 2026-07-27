# Basic vector methods for `mdate` objects

These methods let `mdate` vectors behave like ordinary character vectors
for subsetting, replacement, concatenation, repetition, and
deduplication, while ensuring the result remains a validated `mdate`
object.

## Usage

``` r
# S3 method for class 'mdate'
x[..., drop = TRUE]

# S3 method for class 'mdate'
x[i, ...] <- value

# S3 method for class 'mdate'
x[[...]]

# S3 method for class 'mdate'
x[[i, ...]] <- value

# S3 method for class 'mdate'
c(...)

# S3 method for class 'mdate'
rep(x, ...)

# S3 method for class 'mdate'
unique(x, incomparables = FALSE, ...)

# S3 method for class 'mdate'
duplicated(x, incomparables = FALSE, ...)
```

## Arguments

- x:

  An `mdate` object.

- drop:

  Included for consistency with the default `[` method; has no effect
  since `mdate` objects are always vectors.

- i, ...:

  Index or indices, as for the default methods; for
  [`c()`](https://rdrr.io/r/base/c.html), one or more objects to
  concatenate (coerced to `mdate` first).

- value:

  A replacement value, coerced to `mdate` and validated before
  assignment.

- incomparables:

  Values that cannot be compared, as for the default methods; `FALSE` by
  default.

## Value

An `mdate` object, except for
[`duplicated()`](https://rdrr.io/r/base/duplicated.html), which returns
a logical vector, and [`c()`](https://rdrr.io/r/base/c.html), which
returns the (unclassed) result when called on a single object.

## Details

[`unique()`](https://rdrr.io/r/base/unique.html) and
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) compare the
annotated strings, not the sets of dates they expand to, so two values
that could refer to the same date but are written differently (e.g.
`"2012-01"` and `"2012-01-01..2012-01-31"`) are treated as distinct.

## Examples

``` r
d <- as_messydate(c("2012-01-01", "2012-02-01", "2012-03-01"))
d[2]
#>  'mdate' chr "2012-02-01"
d[2] <- "2012-02-02"
c(d, as_messydate("2012-04-01"))
#>  'mdate' chr [1:4] "2012-01-01" "2012-02-02" "2012-03-01" "2012-04-01"
rep(d, 2)
#>  'mdate' chr [1:6] "2012-01-01" "2012-02-02" "2012-03-01" "2012-01-01" ...
unique(as_messydate(c("2012-01-01", "2012-01-01", "2012-02-01")))
#>  'mdate' chr [1:2] "2012-01-01" "2012-02-01"
```

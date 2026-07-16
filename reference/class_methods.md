# Basic vector methods for `mdate` objects

These methods let `mdate` vectors behave like ordinary character vectors
for subsetting, replacement, concatenation, and repetition, while
ensuring the result remains a validated `mdate` object.

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

## Value

An `mdate` object, except for [`c()`](https://rdrr.io/r/base/c.html),
which returns the (unclassed) result when called on a single object.

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
```

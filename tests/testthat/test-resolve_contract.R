# The rules that every resolution function obeys.
# A new resolution function goes into `elementwise` or `reducers` below,
# and must then pass every test in this file.

elementwise <- list(vmin = vmin, vmax = vmax, vmean = vmean,
                    vmedian = vmedian, vmodal = vmodal, vrandom = vrandom)
reducers <- list(min = min, max = max, mean = mean,
                 median = median, modal = modal, random = random)

# One value of each kind that the resolvers must handle.
contract_x <- as_messydate(c(
  "2012-01-01",                 # precise date
  "2012-01",                    # month precision
  NA,                           # missing
  "{}",                         # empty set
  "2012-01-01..2012-01-03",     # range
  "?2012-06",                   # uncertain
  "2012-06-~15",                # approximate
  "{2012-01-01,2012-03-01}",    # set
  "[2012-01-01,2012-03-01]",    # one of a set
  "2012-XX-15",                 # unspecified month
  "-0050-01",                   # before the common era
  "-0050-01-01..-0050-01-05",   # range before the common era
  "2012-01-01 14:30",           # precise date-time
  "2012-01-01 09:00..2012-01-02 12:00" # range of date-times
))
none <- is.na(contract_x) | is_empty_notation(contract_x)
bce <- date_is_bce(contract_x) %in% TRUE

# Day numbers that also order dates before the common era correctly.
day_number <- function(x) {
  as.numeric(as.Date(new_messydate(strip_times(as.character(x))), FUN = identity))
}

for (nm in names(elementwise)) {
  f <- elementwise[[nm]]

  test_that(paste(nm, "gives an mdate of the same length"), {
    set.seed(1)
    out <- f(contract_x)
    expect_true(is_messydate(out))
    expect_length(out, length(contract_x))
    expect_true(is_messydate(f(new_messydate(character(0)))))
    expect_length(f(new_messydate(character(0))), 0)
  })

  test_that(paste(nm, "gives NA for NA and for the empty set, and only there"), {
    set.seed(1)
    out <- f(contract_x)
    expect_true(all(is.na(out[none])))
    expect_false(any(is.na(out[!none])))
  })

  test_that(paste(nm, "gives precise dates within the bounds of each value"), {
    set.seed(1)
    out <- f(contract_x)[!none]
    expect_true(all(date_is_precise(out)))
    lo <- day_number(vmin(contract_x)[!none])
    hi <- day_number(vmax(contract_x)[!none])
    d <- day_number(out)
    expect_true(all(d >= lo & d <= hi))
  })

  test_that(paste(nm, "keeps the era of a date before the common era"), {
    set.seed(1)
    out <- f(contract_x)
    expect_true(all(date_is_bce(out[bce])))
    expect_false(any(date_is_bce(out[!bce & !none])))
  })

  test_that(paste(nm, "gives a precise value back as it is"), {
    precise <- as_messydate(c("2012-01-01", "2012-01-01 14:30", "-0050-01-01"))
    expect_identical(as.character(f(precise)), as.character(precise))
  })

  test_that(paste(nm, "gives an error for an argument that it does not know"), {
    expect_error(f(contract_x, aprox_range = 3), "aprox_range")
  })
}

for (nm in names(reducers)) {
  f <- reducers[[nm]]

  test_that(paste(nm, "gives one precise mdate within the bounds of the vector"), {
    set.seed(1)
    out <- f(contract_x, na.rm = TRUE)
    expect_true(is_messydate(out))
    expect_length(out, 1)
    expect_true(date_is_precise(out))
    d <- day_number(out)
    expect_true(d >= day_number(min(contract_x, na.rm = TRUE)))
    expect_true(d <= day_number(max(contract_x, na.rm = TRUE)))
  })

  test_that(paste(nm, "gives NA for a vector with no dates"), {
    out <- f(as_messydate(c(NA, "{}")), na.rm = TRUE)
    expect_true(is_messydate(out))
    expect_true(is.na(out))
  })

  test_that(paste(nm, "gives a single precise value back as it is"), {
    expect_identical(as.character(f(as_messydate("2012-01-01 14:30"))),
                     "2012-01-01 14:30")
    expect_identical(as.character(f(as_messydate("-0050-01-01"))), "-0050-01-01")
  })
}

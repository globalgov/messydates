test_that("two-digit years leave the century unspecified by default", {
  expect_equal(as.character(as_messydate("1/1/29")), "XX29-01-01")
  expect_equal(as.character(as_messydate("12/31/99")), "XX99-12-31")
  expect_equal(as.character(as_messydate("31/12/99")), "XX99-12-31")
  expect_equal(as.character(as_messydate("15-Mar-95")), "XX95-03-15")
  expect_equal(as.character(as_messydate("May-97")), "XX97-05")
  expect_equal(as.character(as_messydate("010221", resequence = "dmy")),
               "XX21-02-01")
})

test_that("a pivot places two-digit years in a 100-year window", {
  expect_equal(as.character(as_messydate(c("1/1/29", "1/1/30"), pivot = 1930)),
               c("2029-01-01", "1930-01-01"))
  expect_equal(as.character(as_messydate("May-97", pivot = 1930)), "1997-05")
  expect_equal(as.character(as_messydate("010221", resequence = "dmy",
                                         pivot = 1950)), "2021-02-01")
  expect_error(as_messydate("1/1/29", pivot = 29), "four-digit year")
})

test_that("four-digit years are unaffected by the two-digit rules", {
  expect_equal(as.character(as_messydate(c("1/2/2003", "2019/03/01", "May 1997"))),
               c("2003-02-01", "2019-03-01", "1997-05"))
})

test_that("Excel serial numbers convert with the right origin", {
  expect_equal(as.character(as_messydate(c(1, 59, 61, 36892), origin = "excel")),
               c("1900-01-01", "1900-02-28", "1900-03-01", "2001-01-01"))
  expect_equal(as.character(as_messydate(45000.5, origin = "excel")),
               "2023-03-15 12:00:00")
  expect_warning(x <- as_messydate(60, origin = "excel"), "never existed")
  expect_true(is.na(x))
  expect_equal(as.character(as_messydate(c(0, 1462), origin = "excel1904")),
               c("1904-01-01", "1908-01-02"))
  expect_equal(as.character(as_messydate(19000, origin = "1970-01-01")),
               "2022-01-08")
})

test_that("numbers without an origin are years, YYYYMMDD, or NA with a warning", {
  expect_equal(as.character(as_messydate(c(2021, 20210201))),
               c("2021", "2021-02-01"))
  expect_warning(x <- as_messydate(36892), "origin")
  expect_true(is.na(x))
  expect_equal(as.character(as_messydate(Inf)), "9999-12-31")
})

test_that("a two-digit year written first is kept as written", {
  expect_equal(as.character(as_messydate("39-01-01")), "0039-01-01")
})

test_that("an unspecified century resolves, expands, and compares", {
  x <- as_messydate("XX29-01-01")
  expect_equal(as.character(vmin(x)), "0029-01-01")
  expect_equal(as.character(vmax(x)), "9929-01-01")
  expect_length(possible_dates(x)[[1]], 100)
  expect_equal(possible_dates(x)[[1]][1:2], c("0029-01-01", "0129-01-01"))
  expect_true(is.na(x < as_messydate("2000-01-01")))
  expect_false(x < as_messydate("0010-01-01"))
  expect_length(possible_dates(as_messydate("192X"))[[1]], 3653)
})

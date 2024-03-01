test_that("new_messydate works", {
  expect_equal(unclass(as_messydate("2012-01-01")), "2012-01-01")
  expect_equal(unclass(as_messydate("2012-1-1")), "2012-01-01")
})

test_that("incompleteness works", {
  expect_equal(unclass(as_messydate("NA-01-01")), "XXXX-01-01")
  expect_equal(unclass(as_messydate("NA-NA-01")), "XXXX-XX-01")
  expect_equal(unclass(as_messydate("2012-NA-01")), "2012-XX-01")
  expect_equal(unclass(as_messydate("2012-01-NA")), "2012-01")
  expect_equal(unclass(as_messydate("2012-1")), "2012-01")
  expect_equal(unclass(as_messydate("2012")), "2012")
  expect_equal(unclass(as_messydate("1")), "0001")
  expect_equal(unclass(as_messydate("12-10-93")), "93-10-12")
})

test_that("uncertainty works", {
  expect_equal(unclass(as_messydate("2012?-1-1")), "2012?-01-01")
  expect_equal(unclass(as_messydate("2012-01?-1")), "2012-01?-01")
  expect_equal(unclass(as_messydate("2012-1-01?")), "2012-01-01?")
})

test_that("approximation works", {
  expect_equal(unclass(as_messydate("2012~-1-1")), "2012~-01-01")
  expect_equal(unclass(as_messydate("2012-01~-01")), "2012-01~-01")
  expect_equal(unclass(as_messydate("2012-1-01~")), "2012-01-01~")
})

test_that("ranges work", {
  expect_equal(unclass(as_messydate("2012-01-01:2014-01-01")),
               "2012-01-01..2014-01-01")
  expect_equal(unclass(as_messydate("2012-01-01..2014-01-01")),
               "2012-01-01..2014-01-01")
  expect_equal(unclass(as_messydate("2012-01-01_2014-01-01")),
               "2012-01-01..2014-01-01")
})

test_that("negative works", {
  expect_equal(unclass(as_messydate("28 BC")), "-0028")
  expect_equal(unclass(as_messydate("200 BC:100 BC")), "-0200..-0100")
  expect_equal(unclass(as_messydate("{-200, -100}")), "{-0200,-0100}")
})

test_that("validate_messydate works", {
  expect_equal(validate_messydate(as_messydate("28 BC")), as_messydate("28 BC"))
  expect_error(validate_messydate(as_messydate("28 BCK")))
  expect_error(validate_messydate(as_messydate("X")))
  expect_error(validate_messydate(as_messydate("28>>")))
})

test_that("print method works", {
  expect_output(print(as_messydate("28 BC")), "-0028")
})

test_that("c method works", {
  expect_identical(as_messydate(c("2012-01-01", "2012-06-01")),
                   c(as_messydate("2012-01-01"), as_messydate("2012-06-01")))
})

test_that("subset and subset-assign methods work", {
  strings <- c("2012-01-01", "2012-XX-01", "2012-01-01:2014-01-01", "28 BC")
  full_md <- as_messydate(strings)
  expect_identical(full_md[1], as_messydate(strings[1]))
  expect_identical(full_md[2:4], as_messydate(strings[2:4]))
  expect_identical(full_md[[3]], as_messydate(strings[[3]]))
  x <- full_md
  x[1] <- full_md[4]
  expect_identical(x, as_messydate(strings[c(4, 2:4)]))
  x <- full_md
  x[c(2, 3)] <- strings[c(1, 4)]
  expect_identical(x, as_messydate(strings[c(1, 1, 4, 4)]))
  x <- full_md
  x[[4]] <- full_md[1]
  expect_identical(x, as_messydate(strings[c(1:3, 1)]))
  x <- full_md
  expect_error(x[1] <- "not a date")
  expect_error(x[[1]] <- "not a date")
})

test_that("rep method works", {
  expect_identical(rep(as_messydate(c("10 AD", "20 AD")), 2),
                   as_messydate(c("10 AD", "20 AD", "10 AD", "20 AD")))
  expect_identical(rep(as_messydate(c("10 AD", "20 AD")), each = 2),
                   as_messydate(c("10 AD", "10 AD", "20 AD", "20 AD")))
})

test_that("as.list method works", {
  x <- as_messydate(c(a = "2012-01-01", b = "2012-XX-01"))
  xl <- as.list(x)
  expect_length(xl, length(x))
  expect_identical(xl[[1]], x[[1]])
  expect_identical(xl[[2]], x[[2]])
  expect_identical(names(xl), names(x))
})

test_that("works with data.frames", {
  x <- as_messydate(c("2012-01-01", "2012-XX-01", "2012-01-01:2014-01-01", "28 BC"))
  df_coerce <- as.data.frame(x)
  expect_identical(df_coerce[[1]], x)
  expect_equal(dim(df_coerce), c(4, 1))
  df_call <- data.frame(x)
  expect_equal(df_coerce, df_call)
})

test_that("Annotate functions work properly", {
  data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
                     End = c("1816-12-31", "1916-12-31", "2016-12-31"))
  expect_equal(as.character(on_or_before(data, "Beg", "1816-01-01")),
               c("..1816-01-01", "1916-01-01", "2016-01-01"))
  expect_equal(as.character(on_or_after(data, "End", "2016-01-01")),
               c("1816-12-31", "1916-12-31", "2016-12-31.."))
  expect_equal(as.character(add_approximation(data, "Beg", "1916-01-01")),
               c("1816-01-01", "~1916-01-01", "2016-01-01"))
  expect_equal(as.character(add_approximation(data, "Beg", "1916-01-01", "year")),
               c("1816-01-01", "1916~-01-01", "2016-01-01"))
  expect_equal(as.character(add_approximation(data, "Beg", "1916-01-01", "month")),
               c("1816-01-01", "1916-01~-01", "2016-01-01"))
  expect_equal(as.character(add_approximation(data, "Beg", "1916-01-01", "day")),
               c("1816-01-01", "1916-01-01~", "2016-01-01"))
  expect_equal(as.character(add_uncertainty(data, "End", "1916-12-31")),
               c("1816-12-31", "?1916-12-31", "2016-12-31"))
  expect_equal(as.character(add_uncertainty(data, "End", "1916-12-31", "year")),
               c("1816-12-31", "1916?-12-31", "2016-12-31"))
  expect_equal(as.character(add_uncertainty(data, "End", "1916-12-31", "month")),
               c("1816-12-31", "1916-12?-31", "2016-12-31"))
  expect_equal(as.character(add_uncertainty(data, "End", "1916-12-31", "day")),
               c("1816-12-31", "1916-12-31?", "2016-12-31"))
})

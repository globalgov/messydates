test_that("Annotate functions work properly", {
  data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
                     End = c("1816-12-31", "1916-12-31", "2016-12-31"))
  expect_equal(as.character(on_or_before(data$Beg)),
               c("..1816-01-01", "..1916-01-01", "..2016-01-01"))
  expect_equal(as.character(on_or_after(data$End)),
               c("1816-12-31..", "1916-12-31..", "2016-12-31.."))
  expect_equal(as.character(approximate(data$Beg)),
               c("1816-01-01~", "1916-01-01~", "2016-01-01~"))
  expect_equal(as.character(uncertain(data$End)),
               c("1816-12-31?", "1916-12-31?", "2016-12-31?"))
  expect_equal(as.character(approximate(data$Beg, "year")),
               c("~1816-01-01", "~1916-01-01", "~2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   approximate(data$Beg, "month"),
                                   data$Beg)),
               c("1816-01-01", "1916-~01-01", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   approximate(data$Beg, "day"),
                                   data$Beg)),
               c("1816-01-01", "1916-01-~01", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   approximate(data$Beg, "md"),
                                   data$Beg)),
               c("1816-01-01", "1916-~01-~01", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   approximate(data$Beg, "ym"),
                                   data$Beg)),
               c("1816-01-01", "1916-01~-01", "2016-01-01"))
  expect_equal(as.character(uncertain(data$End, "year")),
               c("?1816-12-31", "?1916-12-31", "?2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                                   uncertain(data$End,
                                                                   "month"),
                                   data$End)),
               c("1816-12-31", "1916-?12-31", "2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                   uncertain(data$End, "day"), data$End)),
               c("1816-12-31", "1916-12-?31", "2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                   uncertain(data$End, "md"), data$End)),
               c("1816-12-31", "1916-?12-?31", "2016-12-31"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   uncertain(data$Beg, "ym"),
                                   data$Beg)),
               c("1816-01-01", "1916-01?-01", "2016-01-01"))
  d <- on_or_before(data$Beg)
  expect_equal(as.character(class(d)), "mdate")
})

test_that("approximate/uncertain error for an unrecognised component", {
  expect_error(approximate("2019-03-01", "century"), "Unknown component")
  expect_error(uncertain("2019-03-01", "century"), "Unknown component")
})

test_that("approximate/uncertain combine", {
  expect_identical(uncertain(approximate("2019-03-01")), mdate("2019-03-01%"))
  expect_identical(approximate(uncertain("2019-03-01")), mdate("2019-03-01%"))
})

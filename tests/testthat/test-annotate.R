test_that("Annotate functions work properly", {
  data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
                     End = c("1816-12-31", "1916-12-31", "2016-12-31"))
  expect_equal(as.character(on_or_before(data$Beg)),
               c("..1816-01-01", "..1916-01-01", "..2016-01-01"))
  expect_equal(as.character(on_or_after(data$End)),
               c("1816-12-31..", "1916-12-31..", "2016-12-31.."))
  expect_equal(as.character(add_approximation(data$Beg)),
               c("~1816-01-01", "~1916-01-01", "~2016-01-01"))
  expect_equal(as.character(add_uncertainty(data$End)),
               c("?1816-12-31", "?1916-12-31", "?2016-12-31"))
  expect_equal(as.character(add_approximation(data$Beg, "year")),
               c("1816~-01-01", "1916~-01-01", "2016~-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   add_approximation(data$Beg, "month"), data$Beg)),
               c("1816-01-01", "1916-01~-01", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   add_approximation(data$Beg, "day"), data$Beg)),
               c("1816-01-01", "1916-01-01~", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   add_approximation(data$Beg, "md"), data$Beg)),
               c("1816-01-01", "1916-~01-01", "2016-01-01"))
  expect_equal(as.character(add_uncertainty(data$End, "year")),
               c("1816?-12-31", "1916?-12-31", "2016?-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                                   add_uncertainty(data$End, "month"), data$End)),
               c("1816-12-31", "1916-12?-31", "2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                   add_uncertainty(data$End, "day"), data$End)),
               c("1816-12-31", "1916-12-31?", "2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                   add_uncertainty(data$End, "md"), data$End)),
               c("1816-12-31", "1916-?12-31", "2016-12-31"))
  d <- on_or_before(data$Beg)
  expect_equal(as.character(class(d)), "messydt")
})

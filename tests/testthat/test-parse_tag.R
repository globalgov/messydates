test_that("Annotate functions work properly", {
  data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
                     End = c("1816-12-31", "1916-12-31", "2016-12-31"))
  expect_equal(as.character(tag_before(data$Beg)),
               c("..1816-01-01", "..1916-01-01", "..2016-01-01"))
  expect_equal(as.character(tag_after(data$End)),
               c("1816-12-31..", "1916-12-31..", "2016-12-31.."))
  expect_equal(as.character(tag_approximate(data$Beg)),
               c("1816-01-~01", "1916-01-~01", "2016-01-~01"))
  expect_equal(as.character(tag_uncertain(data$End)),
               c("1816-12-?31", "1916-12-?31", "2016-12-?31"))
  expect_equal(as.character(tag_approximate(data$Beg, "year")),
               c("~1816-01-01", "~1916-01-01", "~2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   tag_approximate(data$Beg, "month"),
                                   data$Beg)),
               c("1816-01-01", "1916-~01-01", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   tag_approximate(data$Beg, "day"),
                                   data$Beg)),
               c("1816-01-01", "1916-01-~01", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   tag_approximate(data$Beg, "md"),
                                   data$Beg)),
               c("1816-01-01", "1916-~01-~01", "2016-01-01"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   tag_approximate(data$Beg, "ym"),
                                   data$Beg)),
               c("1816-01-01", "1916-01~-01", "2016-01-01"))
  expect_equal(as.character(tag_uncertain(data$End, "year")),
               c("?1816-12-31", "?1916-12-31", "?2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                                   tag_uncertain(data$End,
                                                                   "month"),
                                   data$End)),
               c("1816-12-31", "1916-?12-31", "2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                   tag_uncertain(data$End, "day"), data$End)),
               c("1816-12-31", "1916-12-?31", "2016-12-31"))
  expect_equal(as.character(ifelse(data$End == "1916-12-31",
                                   tag_uncertain(data$End, "md"), data$End)),
               c("1816-12-31", "1916-?12-?31", "2016-12-31"))
  expect_equal(as.character(ifelse(data$Beg == "1916-01-01",
                                   tag_uncertain(data$Beg, "ym"),
                                   data$Beg)),
               c("1816-01-01", "1916-01?-01", "2016-01-01"))
  d <- tag_before(data$Beg)
  expect_s3_class(d, "mdate")
})

test_that("approximate/uncertain error for an unrecognised component", {
  expect_error(tag_approximate("2019-03-01", "century"), "Unknown component")
  expect_error(tag_uncertain("2019-03-01", "century"), "Unknown component")
})

test_that("approximate/uncertain combine", {
  expect_identical(tag_uncertain(tag_approximate("2019-03-01")), mdate("2019-03-%01"))
  expect_identical(tag_uncertain(tag_approximate("2019-03-01", "date"), "date"),
                   mdate("2019-03-01%"))
  expect_identical(tag_approximate(tag_uncertain("2019-03-01")), mdate("2019-03-%01"))
})

test_that("Annotation keeps NA as NA", {
  x <- as_messydate(c("2012-01-01", NA))
  expect_equal(as.character(tag_approximate(x)), c("2012-01-~01", NA))
  expect_equal(as.character(tag_uncertain(x, "day")), c("2012-01-?01", NA))
})

test_that("tag_unspecified replaces the digits of a component", {
  # A trailing unspecified component is written as a shorter date
  expect_equal(as.character(tag_unspecified("2012-06-15", "day")), "2012-06")
  expect_equal(as.character(tag_unspecified("2012-06-15", "month")), "2012-XX-15")
  expect_equal(as.character(tag_unspecified("2012-06-15", "year")), "XXXX-06-15")
  expect_equal(as.character(tag_unspecified("2012-06-~15", "day")), "2012-06-~XX")
  expect_equal(as.character(tag_unspecified("2012-06-15 14:30:05", "minute")),
               "2012-06-15 14:XX:05")
  expect_equal(as.character(tag_unspecified("2010-01-01..2012-06-15", "day")),
               "2010-01..2012-06")
  expect_equal(as.character(tag_unspecified(c("{}", NA, "2012-06-15"), "day")),
               c("{}", NA, "2012-06"))
  expect_error(tag_unspecified("2012-06-15"), "component")
  expect_error(tag_unspecified("2012-06-15", "md"), "component")
  expect_false(date_is_precise(tag_unspecified("2012-06-15", "day")))
})

test_that("tag_before and tag_after make open ranges", {
  expect_equal(as.character(tag_before(c("2010", "2010..2012", "..2010", NA, "{}"))),
               c("..2010", "..2012", "..2010", NA, "{}"))
  expect_equal(as.character(tag_after(c("2010", "2010..2012", "2010..", NA, "{}"))),
               c("2010..", "2010..", "2010..", NA, "{}"))
})

test_that("untag removes the tags that it names", {
  x <- c("2019-03-01%", "2019-~03-01", "..2019-03-01", "2019-03-01..", NA, "{}")
  expect_equal(as.character(untag(x)),
               c("2019-03-01", "2019-03-01", "2019-03-01", "2019-03-01", NA, "{}"))
  expect_equal(as.character(untag("2019-03-01%", "approximate")), "2019-03-01?")
  expect_equal(as.character(untag("2019-03-01%", "uncertain")), "2019-03-01~")
  expect_equal(as.character(untag(c("..2019", "2019.."), "before")),
               c("2019", "2019.."))
  expect_equal(as.character(untag("2012-XX-15")), "2012-XX-15")
  expect_identical(untag(tag_uncertain(tag_approximate("2019-03-01", "day"))),
                   as_messydate("2019-03-01"))
})

test_that("the old annotation names still work with one message", {
  .deprecated_env <- getFromNamespace(".deprecated_env", "messydates")
  rm(list = intersect(c("approximate()", "on_or_before()"), ls(.deprecated_env)),
     envir = .deprecated_env)
  expect_message(x <- approximate("2019-03-01"), "tag_approximate")
  expect_equal(as.character(x), "2019-03-~01")
  expect_no_message(approximate("2019-03-01"))
  expect_message(x <- on_or_before("2019"), "tag_before")
  expect_equal(as.character(x), "..2019")
})

test_that("tags go on the finest component by default", {
  expect_equal(as.character(tag_approximate(c("2012-06-15", "2012-06", "2012"))),
               c("2012-06-~15", "2012-~06", "~2012"))
  expect_equal(as.character(tag_approximate("2012-06-15", "date")), "2012-06-15~")
  expect_equal(as.character(tag_uncertain("2012-06-15 14:30:05", "date")),
               "2012-06?-?15 14:30:05")
})

test_that("a tag that is already there is not added again", {
  expect_equal(as.character(tag_approximate("2012-~06")), "2012-~06")
  expect_equal(as.character(tag_approximate("~2012")), "~2012")
  expect_equal(as.character(tag_approximate("2012-~06", "month")), "2012-~06")
  expect_equal(as.character(tag_approximate("2012-06-15~", "day")), "2012-06-15~")
})

test_that("a wider tag replaces the marks that it covers", {
  expect_equal(as.character(tag_approximate("2012-~06", "date")), "2012-06~")
  expect_equal(as.character(tag_approximate("~2012-06-15", "ym")), "2012-06~-15")
  # ~ and ? on the same component become %
  expect_equal(as.character(tag_uncertain("2012-06-15~")), "2012-06~-%15")
})

test_that("a component that the value does not have is not tagged", {
  expect_equal(as.character(tag_uncertain("~2012", "day")), "~2012")
  expect_equal(as.character(tag_approximate("2012-06-15", "hour")), "2012-06-15")
  expect_equal(as.character(tag_approximate("14:30", "year")), "14:30")
})

test_that("each member of a range or a set is tagged", {
  expect_equal(as.character(tag_approximate("2010..2012-06")), "~2010..2012-~06")
  expect_equal(as.character(tag_approximate("..2012-06")), "..2012-~06")
  expect_equal(as.character(tag_uncertain("{2010,2012-06}", "date")),
               "{?2010,2012-06?}")
})

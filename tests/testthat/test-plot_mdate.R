with_null_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  force(expr)
}

test_that("plot() draws messy dates and returns what it drew", {
  x <- as_messydate(c("2012-03-15", "2012-04", "2012-05-10..2012-06-20",
                      "2012-07-~15", "2012-10?", "{2012-02-01,2012-11-15}",
                      "2012-XX-28", NA, "{}"))
  out <- with_null_device(plot(x, approx_range = 3))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 7)
  expect_false(any(is.na(out$value)))
  expect_equal(out$value[1], "2012-XX-28")
  expect_equal(out$min[out$value == "2012-04"], as.Date("2012-04-01"))
  expect_equal(out$max[out$value == "2012-04"], as.Date("2012-04-30"))
})

test_that("plot() takes labels and keeps the order when sort = FALSE", {
  out <- with_null_device(plot(battles$Date, labels = battles$Battle,
                               sort = FALSE))
  expect_equal(out$label, battles$Battle)
  expect_error(with_null_device(plot(battles$Date, labels = "one")),
               "one label for each element")
})

test_that("plot() draws dates before the common era and date-times", {
  x <- as_messydate(c("-0050-03-01..-0050-05-01", "-0049", "2012-01-01 14:30"))
  expect_no_error(with_null_device(plot(x)))
  expect_error(with_null_device(plot(as_messydate(c(NA, "{}")))),
               "no dates to plot")
})

# Testing proportional methods of messydates

test_that("proportional methods work", {
  expect_equal(class(as_messydate("2012-06-02") %l% as_messydate("2012-06")),
               "numeric")
  expect_equal(class(as_messydate("2012-06") %g%
                       as_messydate("2012-06-10")), "numeric")
  expect_equal(class(as_messydate("2012-06") %le%
                       as_messydate("2012-06-10")), "numeric")
  expect_equal(class(as_messydate("2012-06") %ge%
                       as_messydate("2012-06-10")), "numeric")
  expect_equal(class(as_messydate("2012-06") %><%
                       as_messydate("2012-06-15..2012-07-15")), "numeric")
  expect_equal(class(as_messydate("2012-06") %>=<%
                       as_messydate("2012-06-15..2012-07-15")), "numeric")
  expect_equal(round(as_messydate("2012-06") %l% as_messydate("2012-06-10"), 2), 0.3)
  expect_equal(round(as_messydate("2012-06") %g%
                       as_messydate("2012-06-10"), 2), 0.67)
  expect_equal(round(as_messydate("2012-06") %le%
                       as_messydate("2012-06-10"), 2), 0.33)
  expect_equal(round(as_messydate("2012-06") %ge% as_messydate("2012-06-10"), 2), 0.70)
  expect_equal(round(as_messydate("2012-06") %><%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.52)
  expect_equal(round(as_messydate("2012-06") %>=<%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.53)
})

test_that("proportional methods return 1 or 0 for fully separated dates", {
  expect_equal(as_messydate("2012-01-01") %l% as_messydate("2012-06-10"), 1)
  expect_equal(as_messydate("2012-12-31") %g% as_messydate("2012-06-10"), 1)
  expect_equal(as_messydate("2012-12-31") %l% as_messydate("2012-06-10"), 0)
})

test_that("proportional methods dispatch for Date and POSIXt", {
  expect_equal(as.Date("2012-01-01") %l% as_messydate("2012-06-10"), 1)
  expect_equal(as.POSIXct("2012-01-01", tz = "UTC") %l% as_messydate("2012-06-10"), 1)
  expect_equal(as.Date("2012-12-31") %g% as_messydate("2012-06-10"), 1)
  expect_equal(as.POSIXct("2012-12-31", tz = "UTC") %g% as_messydate("2012-06-10"), 1)
  expect_equal(as.Date("2012-06-20") %ge% as_messydate("2012-06-01..2012-06-20"), 1)
  expect_equal(as.Date("2012-06-01") %le% as_messydate("2012-06-01..2012-06-20"), 1)
  # %><% divides by length(expand(e1)) + 1, so even a single contained day
  # cannot reach a full 1
  expect_equal(as.Date("2012-06-10") %><% as_messydate("2012-06-01..2012-06-20"), 0.5)
  expect_equal(as.Date("2012-06-10") %>=<% as_messydate("2012-06-01..2012-06-20"), 1)
})

test_that("proportional methods error on mismatched vector lengths", {
  expect_error(as_messydate(c("2012-01", "2012-02")) %l% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %g% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %le% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %ge% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %><% as_messydate("2012-06-10"))
  expect_error(as_messydate(c("2012-01", "2012-02")) %>=<% as_messydate("2012-06-10"))
})

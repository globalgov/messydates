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
  expect_equal(as_messydate("2012-06") %l% as_messydate("2012-06-10"), 0.3)
  expect_equal(round(as_messydate("2012-06") %g%
                       as_messydate("2012-06-10"), 2), 0.67)
  expect_equal(round(as_messydate("2012-06") %le%
                       as_messydate("2012-06-10"), 2), 0.33)
  expect_equal(as_messydate("2012-06") %ge% as_messydate("2012-06-10"), 0.70)
  expect_equal(round(as_messydate("2012-06") %><%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.52)
  expect_equal(round(as_messydate("2012-06") %>=<%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.53)
})

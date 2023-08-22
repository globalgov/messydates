# Testing proportional methods of messydates

test_that("proportional methods work", {
  expect_equal(class(as_messydate("2012-06-02") %leq% as_messydate("2012-06")),
               "numeric")
  expect_equal(as_messydate("2012-06") %leq% as_messydate("2012-06-10"), 0.3)
  expect_equal(round(as_messydate("2012-06") %geq%
                       as_messydate("2012-06-10"), 2), 0.67)
  expect_equal(round(as_messydate("2012-06") %leqq%
                       as_messydate("2012-06-10"), 2), 0.33)
  expect_equal(as_messydate("2012-06") %geqq% as_messydate("2012-06-10"), 0.70)
  expect_equal(round(as_messydate("2012-06") %><%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.52)
  expect_equal(round(as_messydate("2012-06") %>=<%
                       as_messydate("2012-06-15..2012-07-15"), 2), 0.53)
})

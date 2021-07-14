test_that("set functions work properly", {
  expect_equal(unclass(md_union(as_messydate("2012-01-01..2012-01-03"),
                                as_messydate("2012-01-03..2012-01-04"))),
               c("2012-01-01", "2012-01-02", "2012-01-03", "2012-01-04"))
  expect_equal(unclass(md_intersect(as_messydate("2012-01-01..2012-01-03"),
                                    as_messydate("2012-01-02"))),
               "2012-01-02")
  expect_equal(unclass(md_multiset(as_messydate("2012-01-01..2012-01-03"),
                                   as_messydate("2012-01-05"))),
               c("2012-01-01", "2012-01-02", "2012-01-03", "2012-01-05"))
})

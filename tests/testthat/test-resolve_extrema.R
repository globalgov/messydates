
test_that("min() finds the earliest value in any position", {
  x <- as_messydate(c("2019-03-01", "2012-06", "2012-06-15", "2019-03-01"))
  expect_equal(as.character(min(x)), "2012-06-01")
  expect_equal(as.character(max(x)), "2019-03-01")
})

test_that("the extrema remove % as well as ~ and ?", {
  x <- as_messydate(c("2019-03-%01", "%2012-06", "2012-06~-%15", "2019-03-01%"))
  expect_equal(as.character(vmin(x)),
               c("2019-03-01", "2012-06-01", "2012-06-15", "2019-03-01"))
  expect_equal(as.character(vmax(x)),
               c("2019-03-01", "2012-06-30", "2012-06-15", "2019-03-01"))
  expect_equal(as.character(min(x)), "2012-06-01")
  expect_equal(as.character(max(x)), "2019-03-01")
})

test_that("contract works properly", {
  d <- expand(as_messydate(c("2010-10", "2010")))
  expect_equal(unclass(contract(d)), c("2010-10", "2010"))
})

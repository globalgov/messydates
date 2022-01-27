d <- as_messydate(c("2001-01-01", "2001-01", "2001",
                    "2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
                    "{2001-01,2001-02-02}", "28 BC", "-2000-01-01"))
e <- expand(d)

test_that("contract works properly", {
  expect_equal(contract(e), d)
})

d <- as_messydate(c("2001-01-01", "2001-01", "2001",
                    "2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
                    "{2001-01,2001-02-02}", "-2000-01-01",
                    "2001-01-01..2001-01-03",
                    "2001-XX-01"))
dd <- as_messydate(c("2001-01-01", "2001-01-01..2001-01-31",
                     "2001-01-01..2001-12-31",
                     "2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
                     "{2001-01-01..2001-01-31,2001-02-02}",
                     "-2000-01-01",
                     "2001-01-01..2001-01-03",
                     # "{2001-01-01, 2001-01-02, 2001-01-03}",
                     "2001-XX-01"))
e <- expand(d)

test_that("contract works properly", {
  expect_equal(contract(e), d)
  expect_equal(contract(e, collapse = FALSE), dd)
  expect_equal(contract("{2001-01-01, 2001-01-02, 2001-01-03}"),
               as_messydate("2001-01-01..2001-01-03"))
})

test_that("contract retains the set type", {
  expect_equal(contract(as_messydate("[2001-01-01,2001-02-02]")),
               as_messydate("[2001-01-01,2001-02-02]"))
  expect_equal(contract(as_messydate("{2001-01-01,2001-02-02}")),
               as_messydate("{2001-01-01,2001-02-02}"))
  expect_equal(contract(as_messydate(c("[2001-01-01,2001-02-02]",
                                       "{2001-03-01,2001-04-02}",
                                       "2001-01-01"))),
               as_messydate(c("[2001-01-01,2001-02-02]",
                              "{2001-03-01,2001-04-02}",
                              "2001-01-01")))
  # a list of dates carries no set type, so it contracts to '{}'
  expect_equal(contract(list(c("2001-01-01", "2001-02-02"))),
               as_messydate("{2001-01-01,2001-02-02}"))
})

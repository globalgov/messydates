test_that("Expand dates works properly for date ranges and unspecified dates", {
  range <- as_messydate("2014-01-01..2014-01-03")
  unspecified <- as_messydate("1999?")
  expect_equal(as.character(expand(range)),
               "c(\"2014-01-01\", \"2014-01-02\", \"2014-01-03\")")
  expect_equal(as.character(expand(unspecified)[[1]][1]), "1996-01-01")
  expect_equal(as.character(expand(unspecified)[[1]][1461]), "1999-12-31")
  expect_length(expand(range), 1)
  expect_length(expand(unspecified), 1)
})

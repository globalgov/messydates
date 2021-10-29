test_that("Expand dates works properly for date ranges and unspecified dates", {
  range <- as_messydate("2014-01-01..2014-01-03")
  uncertain <- as_messydate("2000?")
  approximate <- as_messydate("1999~")
  unspecified <- as_messydate("2008-XX-03")
  set <- as_messydate("{2012-01-01,2012-01-12}")
  expect_equal(as.character(expand(range)),
               "c(\"2014-01-01\", \"2014-01-02\", \"2014-01-03\")")
  expect_equal(as.character(expand(uncertain)[[1]][365]), "2000-12-31") # remove uncertainty & expand to make precise
  expect_equal(as.character(expand(approximate)[[1]][1]), "1995-01-01")
  expect_equal(as.character(expand(approximate, approx_range = 4)[[1]][1461]), "1999-12-31")
  expect_equal(as.character(expand(unspecified)[[1]][1]), "2008-01-03")
  expect_equal(as.character(expand(set)[[1]][2]), "2012-01-12")
  expect_length(expand(range), 1)
  expect_length(expand(unspecified), 1)
})

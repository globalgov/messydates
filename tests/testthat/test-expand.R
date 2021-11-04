test_that("Expand dates works properly for date ranges and unspecified dates", {
  range <- as_messydate("2014-01-01..2014-01-03")
  approximate <- as_messydate("1999~")
  unspecified <- as_messydate("2008-XX-03")
  set <- as_messydate("{2012-01-01,2012-01-12}")
  expect_equal(as.character(expand(range)),
               "c(\"2014-01-01\", \"2014-01-02\", \"2014-01-03\")")
  expect_equal(as.character(expand(approximate)[[1]][1]), "1996-01-02")
  expect_equal(as.character(expand(unspecified)[[1]][1]), "2008-01-03")
  expect_equal(as.character(expand(set)[[1]][1]), "2012-01-01")
  expect_length(expand(range), 1)
  expect_length(expand(unspecified), 1)
})

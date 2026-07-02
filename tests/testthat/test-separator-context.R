# Pins the context-sensitive handling of ':' and '_': they remain range
# separators when used between dates, but ':' inside a time is preserved.

test_that("':' and '_' remain range separators between dates", {
  expect_equal(as.character(as_messydate("2009-01-01:2019-01-01")),
               "2009-01-01..2019-01-01")
  expect_equal(as.character(as_messydate("2009-01-01_2019-01-01")),
               "2009-01-01..2019-01-01")
})

test_that("':' inside a time is not treated as a range", {
  x <- as.character(as_messydate("2019-03-01T14:30:00"))
  expect_false(grepl("\\.\\.", x))
  expect_equal(x, "2019-03-01T14:30:00")
})

test_that("ranges of date-times mix '..' ranges with ':' times", {
  x <- as.character(as_messydate("2019-03-01T09:00..2019-03-01T17:00"))
  expect_equal(lengths(strsplit(x, "\\.\\.")), 2L)
  expect_equal(x, "2019-03-01T09:00..2019-03-01T17:00")
})

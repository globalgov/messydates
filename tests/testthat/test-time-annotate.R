dt <- "2012-02-03T14:30:05"

test_that("time components can be annotated as approximate", {
  expect_equal(as.character(as_approximate(dt, "hour")), "2012-02-03 ~14:30:05")
  expect_equal(as.character(as_approximate(dt, "minute")), "2012-02-03 14:~30:05")
  expect_equal(as.character(as_approximate(dt, "second")), "2012-02-03 14:30:~05")
  expect_equal(as.character(as_approximate(dt, "time")), "2012-02-03 14:30:05~")
})

test_that("time components can be annotated as uncertain", {
  expect_equal(as.character(as_uncertain(dt, "hour")), "2012-02-03 ?14:30:05")
  expect_equal(as.character(as_uncertain(dt, "second")), "2012-02-03 14:30:?05")
})

test_that("date components still annotate with a time present", {
  expect_equal(as.character(as_approximate(dt, "day")), "2012-02-~03 14:30:05")
  expect_equal(as.character(as_approximate(dt, "month")), "2012-~02-03 14:30:05")
  expect_equal(as.character(as_uncertain(dt)), "2012-02-03 14:30:05?")
})

test_that("annotated times round-trip through as_messydate", {
  expect_equal(as.character(as_messydate("2012-02-03T~14:30:05")),
               "2012-02-03 ~14:30:05")
  expect_equal(as.character(as_messydate("2012-02-03T14:30:?05")),
               "2012-02-03 14:30:?05")
  expect_equal(as.character(as_messydate("2012-02-03 ~14:30:05")),
               "2012-02-03 ~14:30:05")
})

test_that("date-only annotation behaviour is unchanged", {
  expect_equal(as.character(as_approximate("1916-01-01", "ym")), "1916-01~-01")
  expect_equal(as.character(as_uncertain("1916-12-31", "day")), "1916-12-?31")
})

test_that("a bare time (no date part) can be annotated", {
  bt <- as_messydate("14:30:05")
  expect_equal(as.character(as_approximate(bt, "hour")), "~14:30:05")
  expect_equal(as.character(as_uncertain(bt, "minute")), "14:?30:05")
  expect_equal(as.character(as_approximate(bt, "second")), "14:30:~05")
  expect_equal(as.character(as_approximate(bt, "time")), "14:30:05~")
  expect_equal(as.character(as_uncertain(bt)), "14:30:05?")
  # a date-component marker has nothing to annotate on a bare time (no-op)
  expect_equal(as.character(as_approximate(bt, "year")), "14:30:05")
})

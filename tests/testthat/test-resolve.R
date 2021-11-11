test_that("Resolve dates works properly for date ranges", {
  range <- as_messydate("2014-01-01..2014-01-31")
  expect_equal(as.character(min(range)), "2014-01-01")
  expect_equal(as.character(max(range)), "2014-01-31")
  expect_equal(as.character(median(range)), "2014-01-16")
  expect_equal(as.character(mean(range)), "2014-01-16")
  expect_equal(as.character(modal(range)), "2014-01-01")
})

test_that("Resolve dates works properly for unspecified dates", {
  unspecified <- as_messydate("1999")
  expect_equal(as.character(min(unspecified)), "1999-01-01")
  expect_equal(as.character(max(unspecified)), "1999-12-31")
  expect_equal(as.character(median(unspecified)), "1999-07-02")
  expect_equal(as.character(mean(unspecified)), "1999-07-02")
  expect_equal(as.character(modal(unspecified)), "1999-01-01")
})

test_that("new_messydate works", {
  expect_equal(unclass(as_messydate("2012-01-01")), "2012-01-01")
  expect_equal(unclass(as_messydate("2012-1-1")), "2012-01-01")
})

test_that("incompleteness works", {
  expect_equal(unclass(as_messydate(NA,"01","01")), "XXXX-01-01")
  expect_equal(unclass(as_messydate(NA,NA,"01")), "XXXX-XX-01")
  expect_equal(unclass(as_messydate("2012",NA,"01")), "2012-XX-01")
  expect_equal(unclass(as_messydate("2012","01",NA)), "2012-01")
  expect_equal(unclass(as_messydate("2012","01")), "2012-01")
  expect_equal(unclass(as_messydate("2012")), "2012")
})

test_that("uncertainty works", {
  expect_equal(unclass(as_messydate("2012?","01","01")), "2012?-01-01")
  expect_equal(unclass(as_messydate("2012","01?","01")), "2012-01?-01")
  expect_equal(unclass(as_messydate("2012","01","01?")), "2012-01-01?")
})

test_that("approximation works", {
  expect_equal(unclass(as_messydate("2012~","01","01")), "2012~-01-01")
  expect_equal(unclass(as_messydate("2012","01~","01")), "2012-01~-01")
  expect_equal(unclass(as_messydate("2012","01","01~")), "2012-01-01~")
})

test_that("ranges work", {
  expect_equal(unclass(as_messydate("2012-01-01:2014-01-01")), "2012-01-01..2014-01-01")
  expect_equal(unclass(as_messydate("2012-01-01..2014-01-01")), "2012-01-01..2014-01-01")
  expect_equal(unclass(as_messydate("2012-01-01_2014-01-01")), "2012-01-01..2014-01-01")
})

# test_that("sets work", {
# })

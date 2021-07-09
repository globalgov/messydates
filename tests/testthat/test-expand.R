test_that("Expand dates works properly for date ranges and unspecified dates", {
  range <- "2014-01-01..2014-01-03"
  unspecified <- "1999"
  expect_equal(as.character(expand.messydt(range)), "c(16071, 16072, 16073)")
  expect_equal(as.character(expand.messydt(unspecified)[[1]][1]), "1999-01-01")
  expect_equal(as.character(expand.messydt(unspecified)[[1]][365]), "1999-12-31")
  expect_length(range, 1)
  expect_length(unspecified, 1)
})

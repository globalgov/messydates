test_that("report function work properly", {
  report <- mreport(battles)
  type <- c("character", "mdate")
  names(type) <- c("Battle", "Date")
  expect_s3_class(report, "mreport")
  expect_equal(report$Types, type)
  expect_length(report, 7)
})

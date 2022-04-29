test_that("dates are properly extracted from text", {
  expect_equal(text_to_date(c("This function was created on the 29 September 2021",
                               "Today is October 12, 2021", "Tomorrow is 13-10-2021")),
               as_messydate(c("29-9-2021", "12-10-2021", "13-10-2021")))
})

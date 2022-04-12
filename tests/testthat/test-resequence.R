data <- data.frame(Sign = c("2000-01-01", "2001-01-01",
                            "2001-01-01..2000-01-01",
                            "2000-01-01", NA, "-01-01"),
                   Force = c("2001-01-01", "2000-01-01",
                             "2001-01-01", NA,
                             "2001-01-01", "9999"))
data2 <- data.frame(Sign = c("2000-01-02", "2001-01-02",
                            "2001-01-02..2000-01-02",
                            "2000-01-02", NA, "-01-02"),
                   Force = c("2001-01-02", "2000-01-02",
                             "2001-01-02", NA,
                             "2001-01-02", "9999"))
result <- as.matrix(unname(data.frame(c("2000-01-01",
                                        "2000-01-01",
                                        "2000-01-01",
                                        "2000-01-01",
                                        NA, "-01-01"),
                                      c("2001-01-01",
                                        "2001-01-01",
                                        "2001-01-01",
                                        NA, "2001-01-01",
                                        "9999"))))
result2 <- as.matrix(unname(data.frame(c("2000-01-02",
                                        "2000-01-02",
                                        "2000-01-02",
                                        "2000-01-02",
                                        NA, "-01-02"),
                                      c("2001-01-02",
                                        "2001-01-02",
                                        "2001-01-02",
                                        NA, "2001-01-02",
                                        "9999"))))
attr(result, "dimnames") <- attr(result2, "dimnames") <- NULL

test_that("dates are parsed correctly", {
  expect_equal(resequence(data, c("Sign", "Force")),
               result)
  expect_equal(resequence(data2, c("Sign", "Force")),
               result2)
})

# Test interleave() helper
dat <- c("1", NA,  "2", NA, "3",  "4", "5")

test_that("missing values dropped", {
  expect_equal(as.character(interleave(1:5, c(2, 4))), dat)
  expect_length(interleave(1:5, c(2, 4)), 7)
})

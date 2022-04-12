d <- tibble::tibble(event = c("Event1", "Event2", "Event3", "Event 4"),
                     messydates = as_messydate(c("2001",
                                                 "2001-01-01..2003-12-30",
                                                 "{2001, 2002, 2003}",
                                                 "33 BC")))

test_that("skimr report works", {
  a <- skimr::skim(d)
  expect_equal(a$skim_type, c("character", "messydt"))
  expect_equal(a$skim_variable, c("event", "messydates"))
  expect_equal(a$n_missing, c(0, 0))
  expect_equal(ncol(a), 13)
})

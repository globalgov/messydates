d <- as_messydate(c("2001-01-01", "2001-01", "2001",
                    "2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
                    "{2001-01,2001-02-02}", "-2000-01-01",
                    "2001-01-01..2001-01-03",
                    "2001-XX-01"))
dd <- as_messydate(c("2001-01-01", "2001-01-01..2001-01-31",
                     "2001-01-01..2001-12-31",
                     "2001-01-01..2001-02-02", "{2001-10-01,2001-10-04}",
                     "{2001-01-01..2001-01-31,2001-02-02}",
                     "-2000-01-01",
                     "2001-01-01..2001-01-03",
                     "2001-XX-01"))
e <- possible_dates(d)

test_that("as_messydate() on a list gives the shortest mdate vector", {
  expect_equal(as_messydate(e), d)
  expect_s3_class(as_messydate(e), "mdate")
  expect_length(as_messydate(e), length(d))
  expect_equal(as_messydate(e, collapse = FALSE), dd)
  expect_equal(as_messydate(list(c("2001-01-01", "2001-01-02", "2001-01-03"))),
               as_messydate("2001-01-01..2001-01-03"))
  expect_equal(as_messydate(list("{2001-01-01, 2001-01-02, 2001-01-03}")),
               as_messydate("2001-01-01..2001-01-03"))
})

test_that("as_messydate() on a list gives {} for an empty element and NA for NA", {
  expect_equal(as.character(as_messydate(list(character(0), NA,
                                              c("2001-01-01", "2001-02-02")))),
               c("{}", NA, "{2001-01-01,2001-02-02}"))
})

test_that("possible_dates() and as_messydate() make a round trip", {
  x <- as_messydate(c("2012-01-01..2012-01-03", "2012-06",
                      "{2012-01-01,2012-01-05}", NA, "{}",
                      "-0050-01-01..-0050-01-03"))
  expect_equal(as_messydate(possible_dates(x)), x)
})

test_that("a list of dates carries no set type, so it contracts to {}", {
  expect_equal(as_messydate(possible_dates(as_messydate("[2001-01-01,2001-02-02]"))),
               as_messydate("{2001-01-01,2001-02-02}"))
})

test_that("the internal contraction of an mdate retains the set type", {
  expect_equal(.contract(as_messydate("[2001-01-01,2001-02-02]")),
               as_messydate("[2001-01-01,2001-02-02]"))
  expect_equal(.contract(as_messydate(c("[2001-01-01,2001-02-02]",
                                        "{2001-03-01,2001-04-02}",
                                        "2001-01-01"))),
               as_messydate(c("[2001-01-01,2001-02-02]",
                              "{2001-03-01,2001-04-02}",
                              "2001-01-01")))
  x <- as_messydate(c("2012-01-01..2012-01-31", NA))
  expect_equal(as.character(.contract(x)), c("2012-01", NA))
})

test_that("expand() and contract() still work, with a message", {
  rm(list = intersect(c("expand()", "contract()"), ls(.deprecated_env)),
     envir = .deprecated_env)
  expect_message(old <- expand(d), "possible_dates")
  expect_equal(old, possible_dates(d))
  expect_message(back <- contract(old), "as_messydate")
  expect_equal(back, d)
  expect_equal(suppressMessages(contract(as_messydate("[2001-01-01,2001-02-02]"))),
               as_messydate("[2001-01-01,2001-02-02]"))
})

test_that("possible_dates() has no 'by' argument", {
  expect_false("by" %in% names(formals(possible_dates)))
  expect_error(possible_dates(d, by = "hour"))
})

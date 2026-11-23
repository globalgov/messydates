# Helpers for test-equivalence.R. data-raw/equivalence_reference.R uses them
# to build the reference file in tests/testthat/fixtures/.

equivalence_inputs <- function() {
  c(
    # plain and reduced precision
    "2019-01-01", "2019-12-31", "2020-02-29", "1066-10-14", "0001-01-01",
    "2019-01", "2019-02", "2020-02", "2019",
    # unspecified components
    "2019-XX-01", "2019-01-XX", "2019-XX-XX", "19XX", "201X", "XXXX-01-01",
    "2020-02-XX",
    # annotations
    "2019-01-01~", "2019-01-01?", "2019-01-01%", "~2019-01", "2019?-01-01",
    "2019-~01-01", "2019-01-?01",
    # ranges, including reduced-precision and annotated ends
    "2019-01-01..2019-01-10", "2019-01..2019-03", "2019..2020",
    "2019-12-25..2020-01-05", "2019-01-01~..2019-01-05",
    "2019-02-27..2019-03-02", "2020-02-27..2020-03-02",
    # open ranges
    "..2019-01-01", "2019-01-01..",
    # sets
    "{2019-01-01,2019-01-05}", "[2019-01-01,2019-01-05]",
    "{2019-01-01,2019-02-01..2019-02-03}", "{2019-01,2019-03}",
    # before the common era
    "-0044-03-15", "-0044-03", "-0044", "-0001-12-30..0001-01-02",
    "-0010-01-01..-0010-01-05", "44 BCE",
    # date-times and offsets
    "2019-01-01 14:30", "2019-01-01T14:30:00", "2019-01-01 14:30:00Z",
    "2019-01-01 14:30:00+02:00", "2019-03-01 09:00..2019-03-01 12:00",
    "2019-01-01 ~14:30", "14:30",
    # other written forms
    "01/02/2019", "2019/01/02", "1 January 2019", "January 2019",
    "circa 1910", "early 1990s", "the 1980s", "before 2019", "after 2019-05",
    "summer 2026", "the 5th week of 2026", "the 103rd day of 2026",
    "between 2010 and 2012", "the twentieth century",
    # empty and unparseable input
    NA, "", "not a date"
  )
}

# Runs `f`, recording its value, warnings and error, so the reference holds
# what a user would see as well as what is returned.
capture <- function(f) {
  warn <- character()
  value <- withCallingHandlers(
    tryCatch(f(), error = function(e) structure(conditionMessage(e),
                                                 class = "captured_error")),
    warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage"))
  list(value = value, warnings = warn)
}

equivalence_outputs <- function(inputs = equivalence_inputs()) {
  per_input <- lapply(inputs, function(s) {
    parsed <- capture(function() as_messydate(s))
    d <- parsed$value
    res <- list(parsed = parsed)
    if (inherits(d, "mdate")) {
      res$expand     <- capture(function() possible_dates(d))
      res$expand_a1  <- capture(function() possible_dates(d, approx_range = 1))
      res$expand_hr  <- capture(function() .possible(d, by = "hour"))
      res$vmin       <- capture(function() vmin(d))
      res$vmax       <- capture(function() vmax(d))
      res$vmean      <- capture(function() vmean(d))
      res$vmedian    <- capture(function() vmedian(d))
      res$vmodal     <- capture(function() vmodal(d))
    }
    res
  })
  names(per_input) <- ifelse(is.na(inputs), "<NA>", inputs)
  # Whole-vector calls, which take a different path from the element-wise ones
  ok <- inputs[!is.na(inputs) & inputs != "" & inputs != "not a date" &
                 !grepl("^[0-9]{2}:", inputs)]
  vec <- capture(function() as_messydate(ok))
  whole <- list(parsed = vec)
  if (inherits(vec$value, "mdate")) {
    v <- vec$value
    whole$expand <- capture(function() possible_dates(v))
    whole$min    <- capture(function() min(v))
    whole$max    <- capture(function() max(v))
    whole$mean   <- capture(function() mean(v))
    whole$median <- capture(function() median(v))
    whole$modal  <- capture(function() modal(v))
  }
  list(per_input = per_input, whole = whole)
}

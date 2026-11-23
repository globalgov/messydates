# Times the parser and the resolution functions on typical inputs, so that a
# change made for speed can be measured before and after.
#   Rscript data-raw/benchmark.R
# Add a row here for any new path that is found to be slow.

suppressMessages(devtools::load_all(quiet = TRUE))

time_it <- function(label, expr) {
  t <- system.time(force(expr))[["elapsed"]]
  cat(sprintf("%-48s %7.2f s\n", label, t))
  invisible(t)
}

set.seed(1)
iso <- format(as.Date("1900-01-01") + sample(0:45000, 6000, TRUE))
mixed <- rep(c("2019-01-01", "circa 1910", "2010-01..2010-03"), 2000)
prose <- sprintf("circa %d", sample(1000:2020, 6000, TRUE))
times <- paste(iso[1:2000], sprintf("%02d:%02d", sample(0:23, 2000, TRUE),
                                    sample(0:59, 2000, TRUE)))
imprecise <- as_messydate(rep(c("2019-01", "2019-01-01..2019-03-01"), 500))

cat("messydates", as.character(utils::packageVersion("messydates")), "\n")
time_it("as_messydate(): 6,000 ISO dates", as_messydate(iso))
time_it("as_messydate(): 6,000 mixed (ISO, prose, range)", as_messydate(mixed))
time_it("as_messydate(): 6,000 different prose dates", as_messydate(prose))
time_it("as_messydate(): 2,000 date-times", as_messydate(times))
time_it("possible_dates(): 1,000 months and ranges", possible_dates(imprecise))
time_it("min(): 1,000 months and ranges", min(imprecise))
time_it("mean(): 1,000 months and ranges", mean(imprecise))
time_it("vmean(): 1,000 months and ranges", vmean(imprecise))

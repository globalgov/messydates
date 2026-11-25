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
time_it("compatible_dates(): 1,000 months and ranges", compatible_dates(imprecise))
time_it("min(): 1,000 months and ranges", min(imprecise))
time_it("mean(): 1,000 months and ranges", mean(imprecise))
time_it("vmean(): 1,000 months and ranges", vmean(imprecise))

# Larger inputs, near the size of an event dataset
big_iso <- format(as.Date("1900-01-01") + sample(0:45000, 60000, TRUE))
big_rng <- as_messydate(paste0(big_iso, "..",
                               format(as.Date(big_iso) + sample(1:60, 60000, TRUE))))
d1 <- as.Date("2000-01-01") + 1:100000
time_it("as_messydate(): 60,000 ISO dates", as_messydate(big_iso))
time_it("compatible_dates(): 60,000 ranges", compatible_dates(big_rng))
time_it("vmin(): 60,000 ranges", vmin(big_rng))
time_it("vmean(): 60,000 ranges", vmean(big_rng))
big_mon <- as_messydate(substr(big_iso[1:20000], 1, 7))
big_dt <- as_messydate(paste0(big_iso[1:5000], " 14:30"))
time_it("vmean(): 20,000 months", vmean(big_mon))
time_it("vmean(): 5,000 date-times", vmean(big_dt))
time_it("%l%: 60,000 pairs of ranges", big_rng %l% rev(big_rng))
time_it("%>=<%: 60,000 pairs of ranges", big_rng %>=<% rev(big_rng))
time_it("Date < Date: 100,000 pairs, messydates loaded", d1 < rev(d1))

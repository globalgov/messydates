# Builds tests/testthat/fixtures/equivalence.rds: the output of the parser and
# of the expansion and resolution functions on a fixed sample of inputs.
# tests/testthat/test-equivalence.R compares current output to this file, so a
# change made for speed or tidiness cannot silently change results.
#
# Rebuild only when a change to output is intended, and say so in NEWS.md:
#   Rscript data-raw/equivalence_reference.R

suppressMessages(devtools::load_all(quiet = TRUE))
source("tests/testthat/helper-equivalence.R")
ref <- equivalence_outputs()
dir.create("tests/testthat/fixtures", showWarnings = FALSE)
saveRDS(ref, "tests/testthat/fixtures/equivalence.rds", version = 2, compress = "xz")
message("Saved reference for ", length(ref$per_input), " inputs")

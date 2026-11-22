# Extracted from test-equivalence.R:11

# test -------------------------------------------------------------------------
ref <- readRDS(test_path("fixtures", "equivalence.rds"))
now <- equivalence_outputs()
expect_identical(names(now$per_input), names(ref$per_input))
for (nm in names(ref$per_input)) {
    expect_identical(now$per_input[[nm]], ref$per_input[[nm]], label = nm)
  }

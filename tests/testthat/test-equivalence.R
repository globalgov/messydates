# Compares current output to a reference saved from an earlier version.
# A failure here means a change altered what users get back. If that change
# is intended, rebuild the reference with data-raw/equivalence_reference.R
# and record the change in NEWS.md.

test_that("output matches the saved reference", {
  ref <- readRDS(test_path("fixtures", "equivalence.rds"))
  now <- equivalence_outputs()
  expect_identical(names(now$per_input), names(ref$per_input))
  for (nm in names(ref$per_input)) {
    expect_identical(now$per_input[[nm]], ref$per_input[[nm]], label = nm)
  }
  for (nm in names(ref$whole)) {
    expect_identical(now$whole[[nm]], ref$whole[[nm]],
                     label = paste("whole vector:", nm))
  }
})

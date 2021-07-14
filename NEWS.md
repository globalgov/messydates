# messydates 0.0.1

## Package

* Setup `{messydates}` package
  * Added `DESCRIPTION` file
  * Added `R` folder
  * Added `LICENSE` file
  * Added `NAMESPACE` file
  * Added `NEWS` file
  * Added `README` file
  * Added `.github` folder
  * Added `CODE_OF_CONDUCT` file
  * Added `CONTRIBUTING` file
  * Added `pull_request_template` file
  * Added `ISSUE_TEMPLATE` folder
  * Added `bug_report` file
  * Added `feature_request` file
  * Added `workflows` folder
  * Added `prchecks` file
  * Added `pushrelease` file
  * Added `prcommands` file
  * Added `tests` folder
  * Added `testthat` folder
  * Added `testthat` file
* Setup pkgdown website
* Added package logo

## Functions

* Added a new messydt class which follows the latest ISO 8601 (2019) standards
  * Added validation checks for messydt class
  * Added print methods for messydt class
* Added `as_messydate()` function to coerce from date objects to messydate
  * `as_messydate()` standardises date order, separators and ambiguity
  * Added date class coercion
  * Added POSIXct class coercion
  * Added POSIXlt class coercion
  * Added character class coercion
* Added functions to coerce from messydate objects to other date classes
  * Added `as.Date.messydt()` for coercing to date class
  * Added `as.POSIXct.messydt()` for coercing to POSIXct class
  * Added `as.POSIXlt.messydt()` for coercing to POSIXlt class
* Added `expand()` function for expanding ranged and uncertain dates
* Added functions to resolve expanded dates
  * Added `min.messydt()` to get minimum value from expanded range
  * Added `max.messydt()` to get maximum value from expanded range
  * Added `median.messydt()` to get median value from expanded range
  * Added `mean.messydt()` to get mean value from expanded range
  * Added `modal.messydt()` to get mode value from expanded range
* Added `contract()` function for contracting expanded dates
* Added extract functions to get particular date components
  * Added `year()` to extract year from date
  * Added `month()` to extract month from date
  * Added `day()` to extract day from date
* Added `make_messydate()` function to get messy dates from multiple columns
* Added set functions for operations in sets of messy dates
  * Added `md_intersect()` to find intersection of sets of messy dates
  * Added `md_union()` to find union of sets of messy dates
  * Added `md_multiset()` to join two sets of messy dates
* Added logical function for various logical tests for messy date objects
  * Added `is_messydate()` to test for messydt class
  * Added `is_intersecting()` to test if dates intersect
  * Added `is_element()` to test for multiple elements in dates 
  * Added `is_similar()` to test for similarities in dates

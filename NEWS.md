# messydates 0.0.1

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
* Added a new messydt which follows the latest ISO 8601 (2019) standards
  * Added validation checks for messydt class
  * Added print methods for messydt class
* Added functions to coerce from date objects to messydate
  * Added `as_messydate.Date()` for date class coercion
  * Added `as_messydate.POSIXct()` for POSIXct class
  * Added `as_messydate.POSIXlt()` for POSIXlt class
  * Added `as_messydate.character()` which standardises date order, separators and ambiguity
* Added functions to coerce from messydate objects to other date classes
  * Added `as.Date.messydt()` for coercing to date class
  * Added `as.POSIXct.messydt()` for coercing to POSIXct class
  * Added `as.POSIXlt.messydt()` for coercing to POSIXlt class
* Added `expand.messydt()` function for expanding ranged and uncertain dates
* Added functions to resolve expanded dates
  * Added `min.messydt()` to get minimum value from expanded range
  * Added `max.messydt()` to get maximum value from expanded range
  * Added `median.messydt()` to get median value from expanded range
  * Added `mean.messydt()` to get mean value from expanded range
  * Added `modal.messydt()` to get mode value from expanded range


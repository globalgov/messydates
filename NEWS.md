# messydates 0.3.0

## Package

* Changed object class name to `mdate`
* Closed #41 by creating `{skimr}` template for "mdate" class

## Functions

* Updated coercion to messy dates
  * Closed #45 by improving how `as_messydate()` re-orders 6 digit date components
  * Added "resequence" argument to `as_messydate()` that allows user to choose component order of ambiguous dates
  * Closed #48 by adding zero padding incomplete date ranges and sets of dates
  * Updated `as_messydate()` to also extract dates from text strings
* Added `is_precise()` function that provides a logical test for precise dates
* Updated messy dates expansion
  * Updated `expand()` to allow for the expansion of incomplete date ranges and sets of dates
  * Closed #49 by updating resolve functions to only expand dates if they are not precise

# messydates 0.2.1

## Package

* Added a vignette for working with the `{messydates}` package

## Functions

* Closed #9 by adding arithmetic operations for working with "messydt" objects
  * Added S3 methods for "+" and "-" operators
  * Added `add()` and `subtract()` helper functions for arithmetic operations
* Updated `expand()` function
  * Closed #31 by updating how approximate dates are expanded to account for leap years 
  * Closed #34 by updating `expand()` to manage negative dates
  * Added `expand_negative_dates()` helper function for expanding ranges of negative dates
* Updated functions that coerce from "messydt" objects to "Date" to manage negative dates
  * Added `negative_dates()` helper function to coerce negative "messydt" dates 
* Closed #39 by updating how resolve mean methods work for negative dates
* Closed #40 by updating contract function to manage the contraction of negative dates
  * Added `compact_negative_dates()` helper function to compact negative date ranges
  * Added `is.sequence()` as a helper function to check if dates are a range
* Updated resequence script to export `interleave()` function

# messydates 0.2.0

## Package
* Added PANARCHIC project details to README file
* Added cheatsheet
* Added a new CSS style to website and updated functions displayed
* Addressed workflow actions issues
  * Updated pushrelease.yml workflow actions file to stop installing 
  `{messydates}` from Github
  * Updated README file to stop installing `{messydates}` from Github
* Fixed Codecov test coverage URL on README file for CRAN submission

## Functions
* Expanded on messydates checks for class validity
* Fixed bugs for `make_messydate()`
* Added annotation functions and standardized annotation so that it is 
consistent with ISO2019E standards.
  * `on_or_before()`
  * `on_or_after()`
  * `as_approximate()`
  * `as_uncertain()` (includes discrimination between month uncertainty and 
  day and month uncertainty)
* Updated `as_messydate()` by adding zero padding for month, day or year
* Updated `resequence()` to work consistently with messydate objects
* Updated `expand()` function to expand imprecise, unspecified, approximate, 
uncertain, and negative dates according to approximate ranges and added tests
* Updated `precision()` to return the lengths of expanded dates
* Updated `median()` in resolve family of functions to work with changes to 
`expand()`
* Added tests for functions
  * Added tests for `expand()`
  * Added tests for `contract()`
  * Added tests for `precision()`
  * Added tests for `coerce_from_messydate()`
  

# messydates 0.1.1

## Package

* Updated README with some more explanation about what the package does/offers
* Fixed URL to the package website

# messydates 0.1.0

## Package

* Updated call to `messydt` class in DESCRIPTION file

## Functions

* Updated documentation for `as_messydate()` functions
* Updated documentation for `expand()` function
* Updated documentation for resolve family of functions
* Updated documentation for coerce from family of functions
* Updated documentation for coerce to family of functions

# messydates 0.0.1

## Package

* Setup `{messydates}` package
  * Added `DESCRIPTION` file
  * Added `R` folder
  * Added `LICENSE` file
  * Added `NAMESPACE` file
  * Added `NEWS` file
  * Added `README` files
  * Added `.github` folder and files
  * Added `tests` folder and files
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
* Added tests for new functions
  * Added tests for messydt class and `às_messydate()` function
  * Added tests for coerce from messy dates functions
  * Added tests for coerce to messy dates functions
  * Added tests for `contract()` function
  * Added tests for `expand()` function
  * Added tests for extract functions
  * Added tests for `make_messydate()` function
  * Added tests for resolve functions
  * Added tests for set functions

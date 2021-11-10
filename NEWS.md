# messydates 0.1.2

## Package
* Added PANARCHIC project details to README file
* Added cheatsheet
* Added a new CSS style to website and updated functions displayed

## Functions
* Expanded on messydates checks for class validity
* Fixed bugs for `make_messydate()`
* Added annotation functions
  *`on_or_before()`
  * `on_or_after()`
  * `as_approximate()`
  * `as_uncertain()` (includes discrimination between month uncertainty and 
  day and month uncertainty)
* Updated `as_messydate()` by adding zero padding for month, day or year
* Updated `resequence()` to work consistently with messydate objects
* Updated `expand()` function to expand imprecise, unspecified, approximate, 
uncertain, and negative dates according to approximate ranges and added tests
* Updated `precision()` to return the lengths of expanded dates
* Updated `median()` in resolve family of functions for precision
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

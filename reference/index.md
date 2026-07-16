# Package index

## Class construction

These functions construct dates in the `mdate` class and durations in
the `mduration` class:

- [`new_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)
  [`validate_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)
  [`make_messydate()`](https://globalgov.github.io/messydates/reference/class_mdate.md)
  [`print(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_mdate.md)
  [`format(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_mdate.md)
  : A flexible date class for messy dates

- [`new_messyduration()`](https://globalgov.github.io/messydates/reference/class_mduration.md)
  [`validate_messyduration()`](https://globalgov.github.io/messydates/reference/class_mduration.md)
  [`make_messyduration()`](https://globalgov.github.io/messydates/reference/class_mduration.md)
  [`print(`*`<mduration>`*`)`](https://globalgov.github.io/messydates/reference/class_mduration.md)
  : A flexible duration class for messy durations

- [`` `[`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_methods.md)
  [`` `[<-`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_methods.md)
  [`` `[[`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_methods.md)
  [`` `[[<-`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_methods.md)
  [`c(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_methods.md)
  [`rep(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/class_methods.md)
  :

  Basic vector methods for `mdate` objects

## Coerce to

These functions coerce dates from other classes to the `mdate` class:

- [`as_messydate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  [`mdate()`](https://globalgov.github.io/messydates/reference/coerce_to.md)
  :

  Coercion from common date classes to `mdate`

## Coerce from

These functions coerce dates from the `mdate` class into a single
`Date`:

- [`as.Date(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  [`as.POSIXct(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  [`as.POSIXlt(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  [`as.data.frame(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  [`as.list(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  [`as.double(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  [`as_datetime(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/coerce_from.md)
  :

  Coercion from `mdate` to common date classes

- [`vmin()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  [`min(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  [`vmax()`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  [`max(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/resolve_extrema.md)
  : Resolves messy dates into an extrema

- [`median(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  [`vmedian()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  [`mean(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  [`vmean()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  [`modal()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  [`vmodal()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  [`random()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  [`vrandom()`](https://globalgov.github.io/messydates/reference/resolve_tendency.md)
  : Resolves messy dates into a central tendency

## Manipulation

These functions expand or contract objects of `mdate` class from/into a
list:

- [`contract()`](https://globalgov.github.io/messydates/reference/convert_contract.md)
  : Contract lists of dates into messy dates
- [`expand()`](https://globalgov.github.io/messydates/reference/convert_expand.md)
  : Expand messy dates to lists of dates
- [`seq(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/convert_sequence.md)
  : Sequence method for messydates

## Components

These functions annotate or extract components of dates and times:

- [`on_or_before()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  [`on_or_after()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  [`approximate()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  [`uncertain()`](https://globalgov.github.io/messydates/reference/component_annotate.md)
  : Annotates dates as censored, uncertain, or approximate
- [`year(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/component_extract.md)
  [`month(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/component_extract.md)
  [`mday(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/component_extract.md)
  [`hour(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/component_extract.md)
  [`minute(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/component_extract.md)
  [`second(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/component_extract.md)
  [`tz(`*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/component_extract.md)
  [`precision()`](https://globalgov.github.io/messydates/reference/component_extract.md)
  : Extracting components from messy dates

## Operations

These methods help operate on objects of the `mdate` class:

- [`` `+`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/operate_arithmetic.md)
  [`` `-`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/operate_arithmetic.md)
  : Arithmetic operations for messydates
- [`` `<`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/operate_inequalities.md)
  [`` `>`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/operate_inequalities.md)
  [`` `<=`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/operate_inequalities.md)
  [`` `>=`( ``*`<mdate>`*`)`](https://globalgov.github.io/messydates/reference/operate_inequalities.md)
  : Logical operations on messy dates
- [`` `%l%` ``](https://globalgov.github.io/messydates/reference/operate_proportional.md)
  [`` `%g%` ``](https://globalgov.github.io/messydates/reference/operate_proportional.md)
  [`` `%ge%` ``](https://globalgov.github.io/messydates/reference/operate_proportional.md)
  [`` `%le%` ``](https://globalgov.github.io/messydates/reference/operate_proportional.md)
  [`` `%><%` ``](https://globalgov.github.io/messydates/reference/operate_proportional.md)
  [`` `%>=<%` ``](https://globalgov.github.io/messydates/reference/operate_proportional.md)
  : Proportion of messy dates meeting logical test
- [`` `%intersect%` ``](https://globalgov.github.io/messydates/reference/operate_set.md)
  [`` `%union%` ``](https://globalgov.github.io/messydates/reference/operate_set.md)
  : Set operations for messy dates
- [`is_messydate()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  [`is_intersecting()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  [`is_subset()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  [`is_similar()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  [`is_precise()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  [`is_uncertain()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  [`is_approximate()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  [`is_bce()`](https://globalgov.github.io/messydates/reference/operate_statements.md)
  : Logical statements on messy dates

## Data

Working with ‘messy’ data:

- [`battles`](https://globalgov.github.io/messydates/reference/battles.md)
  : Dates of battles in 2001

# Dates of battles in 2001

A dataset containing the names and dates of battles in 2001, according
to Wikipedia
(https://en.wikipedia.org/wiki/List_of_battles_in_the_21st_century).

## Usage

``` r
battles
```

## Format

A data frame with 20 rows and 5 variables:

- Battle:

  name of the battle, character

- Date:

  date or date range, a mdate class vector

- Parties:

  parties to the conflict, character

- US_party:

  is the US a party to the battle, numeric

- N_actors:

  number of actors to conflict, numeric

## Examples

``` r
battles$Date
#>  'mdate' chr [1:20] "2001-03-08" "2001-04-16..2001-04-20" "2001-05-25" ...
as.Date(battles$Date, FUN = vmin)
#>  [1] "2001-03-08" "2001-04-16" "2001-05-25" "2001-06-22" "2001-08-13"
#>  [6] "2001-10-07" "2001-10-19" "2001-11-09" "2001-11-11" "2001-11-12"
#> [11] "2001-11-13" "2001-11-13" "2001-11-15" "2001-11-22" "2001-11-25"
#> [16] "2001-12-12" "2001-12-03" "2001-12-04" "2001-12-22" "2001-12-30"
```

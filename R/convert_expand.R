#' Expand messy dates to lists of dates
#' @description
#'   These functions expand on date ranges, sets of dates, and unspecified or
#'   approximate dates (annotated with '..', '{}', 'XX' or '~').
#'   As these messydates may refer to several possible dates,
#'   the function "opens" these values to reveal a vector of all the possible
#'   dates implied.
#'   Imprecise dates (dates only containing information on year and/or month)
#'   are also expanded to include possible dates within that year and/or month.
#'   The function removes the annotation from dates with unreliable sources ('?'),
#'   before being expanded normally as though they were incomplete.
#' @param x A `mdate` object.
#'   If not an 'mdate' object, conversion is handled first with ´as_messydate()´.
#' @param approx_range Range to expand approximate dates,
#'   or date components, annotated with '~', by default 0.
#'   That is, removes signs for approximate dates and
#'   treats these dates as precise dates.
#'   If 3, for example, adds 3 days for day approximation,
#'   3 months for month approximation,
#'   3 years for year/whole date approximation,
#'   3 years and 3 months for year-month approximation,
#'   and 3 months and 3 days for month-day approximation.
#' @return A list of dates, including all dates in each range or set.
#' @importFrom lubridate as_date ymd years
#' @name convert_expand
#' @examples
#' d <- as_messydate(c("2008-03-25", "-2012-02-27", "2001-01?", "~2001",
#' "2001-01-01..2001-02-02", "{2001-01-01,2001-02-02}", "{2001-01,2001-02-02}",
#' "2008-XX-31", "..2002-02-03", "2001-01-03..", "28 BC"))
#' expand(d)
#' @export
expand <- function(x, approx_range = 0) {
  if (!is_messydate(x)) {
    message("Date object(s) converted to 'mdate' class")
    x <- as_messydate(x)
  }
  # remove initial spaces, braces, and uncerainty
  x <- stringi::stri_replace_all_regex(x, "[:space:]|\\{|\\}|\\%|\\?", "")
  if (approx_range == 0) {
    # if no approx_range, then can just ignore these annotations
    x <- stringi::stri_replace_all_regex(x, "\\~|^\\.\\.|\\.\\.$", "")
  } else {
    # otherwise we need to expand approximate dates
    x <- expand_approximate(x, approx_range)
  }
  x <- expand_unspecified(x)
  # x <- expand_negative(x)
  x <- expand_sets(x) # Can create a list..
  x <- expand_ranges(x)
  x
}

## expand approx ####
#' @importFrom stringi stri_detect_regex stri_replace_all_regex
expand_approximate <- function(dates, approx_range) {
  # Substitute signs
  dates <- dplyr::case_when(
    stringi::stri_detect_regex(dates, "^\\~[:digit:]{4}$") ~ paste0(dates, "-01-01"),
    stringi::stri_detect_regex(dates, "^[:digit:]{4}-\\~[:digit:]{2}$|^[:digit:]{4}-[:digit:]{2}\\~$") ~ paste0(dates, "-01"),
    stringi::stri_detect_regex(dates, "\\~") & stringi::stri_detect_regex(dates, "\\.\\.") ~ stringi::stri_replace_all_regex(dates, "\\~", ""),
    .default = dates
  )
  # expansion for approximate ranges not yet implemented
  dates <- suppressWarnings(expand_approximate_years(dates, approx_range))
  dates <- suppressWarnings(expand_approximate_months(dates, approx_range))
  dates <- suppressWarnings(expand_approximate_days(dates, approx_range))
  dates <- unlist(dates)
  dates
}

expand_approximate_years <- function(dates, approx_range) {
  # For year approximation
  ly <- as.numeric(strsplit(as.character(approx_range / 4), "\\.")[[1]][1]) +
    (365 * approx_range)
  dates <- lapply(dates, function(x) {
    asdat <- as.Date(gsub("\\~", "", x))

    # Leap year
    x <- ifelse(stringi::stri_detect_regex(x, "^\\~[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                                    |^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}\\~$") &
                  approx_range < 4 &
                  lubridate::leap_year(lubridate::as_date(gsub("\\~", "", x))),
                paste0(asdat - ly, "..",
                       asdat + ly + 1), x)
    # Non leap year
    x <- ifelse(stringi::stri_detect_regex(x, "^\\~[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$|
                                    |^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}\\~$"),
                paste0(asdat - ly, "..",
                       asdat + ly), x)
    # On before
    x <- ifelse(stringi::stri_detect_regex(x, "^\\.\\."),
                paste0(as.Date(gsub("\\.\\.", "", x)) - ly, "..",
                       gsub("\\.\\.", "", x)), x)
    # On after, leap
    x <- ifelse(stringi::stri_detect_regex(x, "\\.\\.$") & approx_range < 4 &
                  lubridate::leap_year(lubridate::as_date(gsub("\\.\\.",
                                                               "", x))),
                paste0(gsub("\\.\\.", "", x), "..",
                       as.Date(gsub("\\.\\.", "", x)) + ly + 1), x)
    # On after
    x <- ifelse(stringi::stri_detect_regex(x, "\\.\\.$"),
                paste0(gsub("\\.\\.", "", x), "..", as.Date(gsub("\\.\\.", "", x)) + ly), x)
    # Year-Month
    x <- ifelse(stringi::stri_detect_regex(x, "^[:digit:]{4}-[:digit:]{2}\\~-[:digit:]{2}$"),
                paste0(asdat -
                         (ly + (30.42 * approx_range)), "..",
                       asdat +
                         (ly + (30.42 * approx_range))), x)
  })
  dates
}

expand_approximate_months <- function(dates, approx_range) {
  # For month approximation
  mr <- 30.42 * approx_range
  dates <- lapply(dates, function(x) {
    asdat <- as.Date(gsub("\\~", "", x))
    # One Month
    x <- ifelse(approx_range == 1 &
                  stringi::stri_detect_regex(x, "-\\~04-|-\\~06-|-\\~09-|-\\~11-"),
                paste0(asdat - 31, "..",
                       asdat + 30), x)
    x <- ifelse(approx_range == 1 &
                  stringi::stri_detect_regex(x, "-\\~05-|-\\~10-|-\\~12-|-\\~07-"),
                paste0(asdat - 30, "..",
                       asdat + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringi::stri_detect_regex(x, "-\\~01-|-\\~08-"),
                paste0(asdat - 31, "..",
                       asdat + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringi::stri_detect_regex(x, "-\\~03-") & # leap year
                  lubridate::leap_year(lubridate::as_date(gsub("\\~", "", x))),
                paste0(asdat - 29, "..",
                       asdat + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringi::stri_detect_regex(x, "-\\~03-"),
                paste0(asdat - 28, "..",
                       asdat + 31), x)
    x <- ifelse(approx_range == 1 &
                  stringi::stri_detect_regex(x, "-\\~02-") & # leap year
                  lubridate::leap_year(lubridate::as_date(gsub("\\~", "", x))),
                paste0(asdat - 31, "..",
                       asdat + 29), x)
    x <- ifelse(approx_range == 1 & stringi::stri_detect_regex(x, "-\\~02-"),
                paste0(asdat - 31, "..",
                       asdat + 28), x)
    # Multiple months
    x <- ifelse(stringi::stri_detect_regex(x, "^[:digit:]{4}-\\~[:digit:]{2}-[:digit:]{2}$"),
                paste0(asdat - mr, "..",
                       asdat + mr), x)
    # Month-Day
    x <- ifelse(stringi::stri_detect_regex(x, "^[:digit:]{4}-\\~[:digit:]{2}-\\~[:digit:]{2}$"),
                paste0(asdat - (mr + approx_range), "..",
                       asdat + (mr + approx_range)), x)
  })
  dates
}

expand_approximate_days <- function(dates, approx_range) {
  dates <- lapply(dates, function(x) {
    asdat <- as.Date(gsub("\\~", "", x))
    # Day
    x <- ifelse(stringi::stri_detect_regex(x, "^[:digit:]{4}-[:digit:]{2}-\\~[:digit:]{2}$"),
                paste0(asdat - approx_range, "..",
                       asdat + approx_range), x)
  })
  dates
}

## expand unspecified ####

#' @importFrom stringi stri_replace_all_fixed
expand_unspecified <- function(dates) {
  # Assumes no century for ambiguous dates not specified previously when dates were coerced
  # dates <- zero_padding(dates)
  dates <- add_zero_padding(dates)
  # Separate ranges and sets of dates
  dates <- stringi::stri_replace_all_fixed(dates, ",", ",,")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})($|,)",
                                    "$1$2-01-01..$2-12-31$3")
  dates <- unspecified_months(dates)
  dates <- stringi::stri_replace_all_fixed(dates, ",,", ",")
  dates
}

zero_padding <- function(y) {
  y <- ifelse(stringi::stri_detect_regex(y, "^\\-([:digit:]{1})-([:digit:]{2})-([:digit:]{2})$"),
              stringi::stri_replace_all_regex(y, "^-", "-000"),
              ifelse(stringi::stri_detect_regex(y, "^\\-([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$"),
                     stringi::stri_replace_all_regex(y, "^-", "-00"),
                     ifelse(stringi::stri_detect_regex(y, "^\\-([:digit:]{3})-([:digit:]{2})-([:digit:]{2})$"),
                            stringi::stri_replace_all_regex(y, "^-", "-0"), y)))
  y <- ifelse(stringi::stri_detect_regex(y, "^([:digit:]{3})-([:digit:]{2})-([:digit:]{2})$"),
              paste0("0", y),
              ifelse(stringi::stri_detect_regex(y, "^([:digit:]{2})-([:digit:]{2})-([:digit:]{2})$"),
                     paste0("00", y),
                     ifelse(stringi::stri_detect_regex(y, "^([:digit:]{1})-([:digit:]{2})-([:digit:]{2})$"),
                            paste0("000", y), y)))
  y
}

unspecified_months <- function(dates) {
  dates <- ifelse(stringi::stri_detect_regex(dates, "(^|,)(-?[:digit:]{4})-02($|,)") &
                    !grepl("\\.", as.numeric(stringi::stri_extract_first_regex(dates, "[:digit:]{4}")) / 4),
                  stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})-02($|,)",
                                           "$1$2-02-01..$2-02-29$3"),
                  stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})-02($|,)",
                                           "$1$2-02-01..$2-02-28$3"))
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})-09($|,)",
                                    "$1$2-09-01..$2-09-30$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})-04($|,)",
                                    "$1$2-04-01..$2-04-30$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})-06($|,)",
                                    "$1$2-06-01..$2-06-30$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})-11($|,)",
                                    "$1$2-11-01..$2-11-30$3")
  dates <- stringi::stri_replace_all_regex(dates,
                                    "(^|,)(-?[:digit:]{4})-([:digit:]{2})($|,)",
                                    "$1$2-$3-01..$2-$3-31$4")
  dates <- stringi::stri_replace_all_regex(dates,
                                    "^(-?[:digit:]{4})-([:digit:]{2})\\.\\.",
                                    "$1-$2-01..")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.(-?[:digit:]{4})-02$",
                                    "..$1-02-28")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.(-?[:digit:]{4})-04$",
                                    "..$1-04-30")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.(-?[:digit:]{4})-06$",
                                    "..$1-06-30")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.(-?[:digit:]{4})-09$",
                                    "..$1-09-30")
  dates <- stringi::stri_replace_all_regex(dates, "\\.\\.(-?[:digit:]{4})-11$",
                                    "..$1-11-30")
  dates <- stringi::stri_replace_all_regex(dates,
                                    "\\.\\.(-?[:digit:]{4})-([:digit:]{2})$",
                                    "..$1-$2-31")
  dates
}

## expand negative ####

expand_negative <- function(dates) {
  dates <- stringi::stri_replace_all_regex(dates, ",", ",,")
  dates <- zero_padding(dates)
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})($|,)",
                                    "$1$2-01-01..$2-12-31$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)-([:digit:]{4})-02($|,)",
                                    "$1$2-02-01%$2-02-28$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)-([:digit:]{4})-04($|,)",
                                    "$1$2-04-01%$2-04-30$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)-([:digit:]{4})-06($|,)",
                                    "$1$2-06-01%$2-06-30$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)-([:digit:]{4})-09($|,)",
                                    "$1$2-09-01%$2-09-30$3")
  dates <- stringi::stri_replace_all_regex(dates, "(^|,)(-?[:digit:]{4})-11($|,)",
                                    "$1$2-11-01..$2-11-30$3")
  dates <- stringi::stri_replace_all_regex(dates,
                                    "(^|,)(-?[:digit:]{4})-([:digit:]{2})($|,)",
                                    "$1$2-$3-01..$2-$3-31$4")
  dates <- stringi::stri_replace_all_regex(dates, "-,", "-")
  dates <- stringi::stri_replace_all_regex(dates, ",,", ",")
  dates
}

## expand sets ####

expand_sets <- function(dates) {
  # Sets of months
  dates <- ifelse(stringi::stri_detect_regex(dates, "^[:digit:]{4}-XX-31$|^[:digit:]{4}-XX-30$"),
                  paste(gsub("XX-31|XX-30", "01-31", dates),
                        gsub("XX-31|XX-30", "02-28", dates),
                        gsub("XX-31|XX-30", "03-31", dates),
                        gsub("XX-31|XX-30", "04-30", dates),
                        gsub("XX-31|XX-30", "05-31", dates),
                        gsub("XX-31|XX-30", "06-30", dates),
                        gsub("XX-31|XX-30", "07-31", dates),
                        gsub("XX-31|XX-30", "08-31", dates),
                        gsub("XX-31|XX-30", "09-30", dates),
                        gsub("XX-31|XX-30", "10-31", dates),
                        gsub("XX-31|XX-30", "11-30", dates),
                        gsub("XX-31|XX-30", "12-31", dates),
                        sep = ","), dates)
  dates <- ifelse(stringi::stri_detect_regex(dates, "^[:digit:]{4}-XX-[:digit:]{2}$"),
                  paste(gsub("XX", "01", dates), gsub("XX", "02", dates),
                        gsub("XX", "03", dates), gsub("XX", "04", dates),
                        gsub("XX", "05", dates), gsub("XX", "06", dates),
                        gsub("XX", "07", dates), gsub("XX", "08", dates),
                        gsub("XX", "09", dates), gsub("XX", "10", dates),
                        gsub("XX", "11", dates), gsub("XX", "12", dates),
                        sep = ","), dates)
  dates <- stringi::stri_split_regex(dates, "\\,")
  dates
}

## expand ranges ####

#' @importFrom purrr map
expand_ranges <- function(dates) {

  purrr::map(dates, function(x)
    unlist(purrr::map(x, function(y)
      if(is_precise(y)) as.character(y) else
        as.character(seq(mdate(y)))))
  )

  # unlist(purrr::map(x[[7]], function(y) if(is_precise(y)) as.character(y) else as.character(seq(mdate(y)))))
  # lapply(x, function(y) if(any(is_precise(y))) else y)


  # dates <- dplyr::case_when(
  #   stringi::stri_detect_regex(dates, "([:digit:]{1})\\.\\.([:digit:]{1})|([:digit:]{1})\\.\\.-") & nchar(dates) < 17 ~ expand_unspecified_ranges(dates),
  #   .default = dates
  #   )
  #
  # dates <- purrr::modify_if(dates, stringi::stri_detect_regex(dates, "([:digit:]{1})\\.\\.([:digit:]{1})|([:digit:]{1})\\.\\.-"), function(x) expand_unspecified_ranges(x))
  #
  #                           stringi::stri_detect_regex(dates, "([:digit:]{1})\\.\\.([:digit:]{1})|([:digit:]{1})\\.\\.-") & nchar(dates) < 17)

  # dates <- suppressWarnings(ifelse(stringi::stri_detect_regex(dates, "([:digit:]{1})\\.\\.([:digit:]{1})|([:digit:]{1})\\.\\.-") &
  #                                    nchar(dates) < 17,
  #                                  expand_unspecified_ranges(dates), dates))

  # dates <- stringi::stri_split_regex(dates, "\\.\\.")
  # dates <- purrr::modify_if(dates, lengths(dates)==2, function(y) if(nchar(y[1]) >0 & nchar(y[2])>0) seq(as.Date(y[1]), as.Date(y[2]), by = "days") else y)

  # dates <- suppressWarnings(lapply(dates, function(x) {
  #   x <- stringi::stri_split_regex(x, "\\.\\.")
  #   if(any(is_bce(x))){
  #     x <- ifelse(stringi::stri_detect_regex(x, "\\%"),
  #                 expand_negative_dates(x), x)
  #   } else {
  #     x <- lapply(x, function(y) {
  #       if (length(y) == 2) y <- as.character(seq(as.Date(y[1]), as.Date(y[2]),
  #                                                 by = "days"))
  #       y
  #     })
  #     unlist(x)
  #   }
  # }))
  # dates
}

# expand_negative_dates <- function(dates) {
#   dates <- lapply(dates, function(x) {
#     x <- stringi::stri_split_regex(x, "\\%")
#     x <- lapply(x, function(a) stringi::stri_replace_all_regex(a, "^-", ""))
#     x <- lapply(x, function(r) {
#       y <- stringi::stri_extract_all_regex(r, "^[0-9]{4}")
#       md <- stringi::stri_replace_first_regex(r, "^[0-9]{4}", "0000")
#       r <- lubridate::ymd(md) - lubridate::years(y)
#       r <- lubridate::as_date(r)
#       r
#     })
#     x <- lapply(x, function(y) {
#       if (length(y) == 2) y <- as.character(seq(y[1], y[2], by = "days"))
#       y
#     })
#     x <- lapply(x, function(y) zero_padding(y))
#   })
#   dates
# }
#
# expand_unspecified_ranges <- function(dates) {
#   dates <- strsplit(as.character(dates), "\\.\\.")
#   dates1 <- purrr::map_chr(dates, 1)
#   dates1 <- ifelse(stringi::stri_detect_regex(dates1,
#                                        "^([:digit:]{4})$|^-([:digit:]{4})$"),
#                   paste0(dates1, "-01-01"), dates1)
#   dates1 <- ifelse(stringi::stri_detect_regex(dates1, "^([:digit:]{4})-([:digit:]{2})$|^-([:digit:]{4})-([:digit:]{2})$"),
#                    paste0(dates1, "-01"), dates1)
#   dates2 <- purrr::map_chr(dates, 2)
#   dates2 <- ifelse(stringi::stri_detect_regex(dates2,
#                                        "^([:digit:]{4})$|^-([:digit:]{4})$"),
#                    paste0(dates2, "-12-31"), dates2)
#   dates2 <- ifelse(stringi::stri_detect_regex(dates2, "^([:digit:]{4})-02$|^-([:digit:]{4})-02$"),
#                    paste0(dates2, "-28"), dates2)
#   dates2 <- ifelse(stringi::stri_detect_regex(dates2,
#                                        "^([:digit:]{4})-01$|^-([:digit:]{4})-01$|
#                                        |^([:digit:]{4})-03$|^-([:digit:]{4})-03$|
#                                        |^([:digit:]{4})-05$|^-([:digit:]{4})-05$|
#                                        |^([:digit:]{4})-07$|^-([:digit:]{4})-07$|
#                                        |^([:digit:]{4})-08$|^-([:digit:]{4})-08$|
#                                        |^([:digit:]{4})-10$|^-([:digit:]{4})-10$|
#                                        |^([:digit:]{4})-12$|^-([:digit:]{4})-12$"),
#                    paste0(dates2, "-31"), dates2)
#   dates2 <- ifelse(stringi::stri_detect_regex(dates2,
#                                        "^([:digit:]{4})-04$|^-([:digit:]{4})-04$|
#                                        |^([:digit:]{4})-06$|^-([:digit:]{4})-06$|
#                                        |^([:digit:]{4})-09$|^-([:digit:]{4})-09$|
#                                        |^([:digit:]{4})-11$|^-([:digit:]{4})-11$"),
#                    paste0(dates2, "-30"), dates2)
#   dates <- paste(dates1, dates2, sep = "..")
#   dates <- ifelse(stringi::stri_detect_regex(dates, "^-|\\.\\.-"),
#                   gsub("\\.\\.", "%", dates), dates)
#   dates
# }
#

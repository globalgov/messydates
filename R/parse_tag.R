#' Tag dates as approximate, uncertain, unspecified, or open
#'
#' @description
#' Sources often give dates as if they were precise when they are not.
#' For example, a dataset can have an arbitrary cut-off point for start
#' and end dates.
#' The `tag_*()` functions add the ISO 8601-2:2019(E) notation for
#' this information to dates.
#' Each `tag_*()` function has a matching `date_is_*()` test
#' (see [operate_statements]).
#'
#' - `tag_approximate()` adds a tilde, `~`, for a date or component that
#'   is possibly correct (e.g. `2003-03-~03` or `2003-03-03~`).
#'   Test with `date_is_approximate()`.
#' - `tag_uncertain()` adds a question mark, `?`, for a date or component
#'   that is possibly incorrect (e.g. `1916-10-?10` or `1916-10-10?`).
#'   A date that is approximate and uncertain gets `%`.
#'   Test with `date_is_uncertain()`.
#' - `tag_unspecified()` replaces the digits of a component with `X`
#'   (e.g. `2012-XX-15`). This discards the digits.
#'   A trailing unspecified component is kept as `XX`,
#'   so `tag_unspecified("2012-06-15", "day")` gives `2012-06-XX`.
#'   Test with `!date_is_precise()`.
#' - `tag_before()` makes a date into an open range that means
#'   "on or before" this date (e.g. `..1816-01-01`).
#'   It does not mean "strictly before".
#'   Test with `date_is_open()`.
#' - `tag_after()` makes a date into an open range that means
#'   "on or after" this date (e.g. `2016-12-31..`).
#'   Test with `date_is_open()`.
#' - `untag()` removes the tags that `tags` names.
#'   It cannot remove `X`, because `tag_unspecified()` discards the digits.
#'
#' The empty set, `{}`, and `NA` do not change.
#' @param x A date vector.
#' @param component The component to tag:
#'   "year", "month" or "day", or a group of date components:
#'   month and day ("md"), year and month ("ym"), or all of them ("date").
#'   For date-times, `component` can also be "hour", "minute", or "second",
#'   or "time" for the whole time of day.
#'   By default, `tag_approximate()` and `tag_uncertain()` tag the finest
#'   component of each value, for example the day of `2012-06-15`
#'   (`2012-06-~15`) or the month of `2012-06` (`2012-~06`).
#'   Use `component = "date"` to tag the whole date (`2012-06-15~`).
#'   A component that the value does not have is not tagged,
#'   so `tag_uncertain("2012", "day")` gives `2012`,
#'   and nor is an unspecified component,
#'   so `tag_approximate("1993-01-XX")` gives `1993-~01-XX`.
#'   A tag that is already there is not added again,
#'   and `~` and `?` on the same component become `%`.
#'   The result uses the fewest marks that tag the same components.
#'   Each end of a range and each member of a set is tagged.
#'   `tag_unspecified()` needs a component, and it does not accept
#'   "md", "ym", "date", or "time".
#' @param tags The tags that `untag()` removes: any of "approximate",
#'   "uncertain", "before", and "after". The default removes all of them.
#' @return An `mdate` object.
#' @examples
#' data <- data.frame(Beg = c("1816-01-01", "1916-01-01", "2016-01-01"),
#'   End = c("1816-12-31", "1916-12-31", "2016-12-31"))
#' transform(data, Beg = ifelse(Beg <= "1816-01-01",
#'   as.character(tag_before(Beg)), Beg))
#' transform(data, End = ifelse(End >= "2016-01-01",
#'   as.character(tag_after(End)), End))
#' transform(data, Beg = ifelse(Beg == "1916-01-01",
#'   as.character(tag_approximate(Beg)), Beg))
#' transform(data, End = ifelse(End == "1916-12-31",
#'   as.character(tag_uncertain(End)), End))
#' tag_approximate("2012-06-15")
#' tag_approximate("2012-06-15", "date")
#' tag_uncertain(tag_approximate("2012-06-15"))
#' tag_unspecified("2012-06-15", "day")
#' # time components can be tagged too
#' tag_approximate("2019-03-01 14:30:00", "hour")
#' tag_uncertain("2019-03-01 14:30:00", "second")
#' # remove tags
#' untag(c("2019-03-01%", "..2019-03-01"))
#' untag("2019-03-01%", "approximate")
#' @name parse_tag
NULL

#' @rdname parse_tag
#' @export
tag_approximate <- function(x, component = NULL) {
  annotate_component(x, component, "~")
}

#' @rdname parse_tag
#' @export
tag_uncertain <- function(x, component = NULL) {
  annotate_component(x, component, "?")
}

#' @rdname parse_tag
#' @export
tag_unspecified <- function(x, component) {
  units <- c("year", "month", "day", "hour", "minute", "second")
  if (missing(component) || length(component) != 1 ||
      !component %in% units)
    stop("`component` must be one of: ", paste(units, collapse = ", "), ".",
         call. = FALSE)
  if (.has_missing(x))
    return(.with_missing(x, tag_unspecified, component = component))
  x <- as.character(x)
  # A set is left as it is: its members can have different precisions.
  set <- grepl("[{}\\[\\],]", x)
  x[!set] <- vapply(strsplit(x[!set], "..", fixed = TRUE), function(ends) {
    paste(vapply(ends, .unspecify_one, character(1), component = component,
                 USE.NAMES = FALSE), collapse = "..")
  }, character(1))
  as_messydate(x)
}

# Replaces the digits of one component of one date or date-time with X.
# Tags such as ~ and ? stay where they are.
.unspecify_one <- function(x, component) {
  if (!nzchar(x)) return(x)
  bare <- grepl("^[~?%]?[0-9X]{1,2}:", x)
  has_t <- grepl("[T ]", x)
  date <- if (bare) "" else sub("[T ].*$", "", x)
  time <- if (bare) x else if (has_t) sub("^[^T ]*[T ]", "", x) else ""
  sep <- if (has_t) regmatches(x, regexpr("[T ]", x)) else ""
  blank <- function(v) gsub("[0-9]", "X", v)
  if (component %in% c("year", "month", "day") && nzchar(date)) {
    neg <- startsWith(date, "-")
    dp <- strsplit(sub("^-", "", date), "-")[[1]]
    i <- match(component, c("year", "month", "day"))
    if (i <= length(dp)) dp[i] <- blank(dp[i])
    date <- paste0(if (neg) "-" else "", paste(dp, collapse = "-"))
  } else if (component %in% c("hour", "minute", "second") && nzchar(time)) {
    off <- regmatches(time, regexpr("(Z|[+-][0-9]{2}:[0-9]{2})$", time))
    core <- sub("(Z|[+-][0-9]{2}:[0-9]{2})$", "", time)
    tp <- strsplit(core, ":")[[1]]
    i <- match(component, c("hour", "minute", "second"))
    if (i <= length(tp)) tp[i] <- blank(tp[i])
    time <- paste0(paste(tp, collapse = ":"), if (length(off)) off else "")
  }
  if (bare) time else if (nzchar(time)) paste0(date, sep, time) else date
}

#' @rdname parse_tag
#' @export
tag_before <- function(x) {
  .tag_open(x, before = TRUE)
}

#' @rdname parse_tag
#' @export
tag_after <- function(x) {
  .tag_open(x, before = FALSE)
}

# Makes a date into an open range. A closed range keeps the end that
# bounds the open range: "on or before" a range is on or before its end.
# A value that is already open, and a set, stay as they are.
.tag_open <- function(x, before) {
  if (.has_missing(x))
    return(.with_missing(x, .tag_open, before = before))
  x <- as.character(x)
  keep <- grepl("^\\.\\.|\\.\\.$|[{}\\[\\],]", x)
  range <- !keep & grepl("..", x, fixed = TRUE)
  x[range] <- if (before) sub("^.*\\.\\.", "", x[range]) else
    sub("\\.\\..*$", "", x[range])
  x[!keep] <- if (before) paste0("..", x[!keep]) else paste0(x[!keep], "..")
  as_messydate(x)
}

#' @rdname parse_tag
#' @export
untag <- function(x, tags = c("approximate", "uncertain", "before", "after")) {
  tags <- match.arg(tags, several.ok = TRUE)
  if (.has_missing(x))
    return(.with_missing(x, untag, tags = tags))
  x <- as.character(x)
  approx <- "approximate" %in% tags
  uncert <- "uncertain" %in% tags
  # "%" is approximate and uncertain together, so removing one of them
  # leaves the other.
  if (approx && uncert) x <- gsub("[~?%]", "", x)
  else if (approx) x <- gsub("~", "", gsub("%", "?", x, fixed = TRUE))
  else if (uncert) x <- gsub("?", "", gsub("%", "~", x, fixed = TRUE),
                             fixed = TRUE)
  if ("before" %in% tags) x <- sub("^\\.\\.", "", x)
  if ("after" %in% tags) x <- sub("\\.\\.$", "", x)
  as_messydate(x)
}

# Adds an annotation mark (~ or ?) to a component of each date or date-time.
# The function reads the marks that are already on each component, adds the
# new mark, and writes the shortest notation that marks the same components.
# A mark that is already there is not added again, and ~ and ? on the same
# component become %. Each member of a range or a set is tagged.
annotate_component <- function(x, component, mark) {
  units <- c("year", "month", "day", "md", "ym", "date",
             "hour", "minute", "second", "time")
  if (!is.null(component) &&
      (length(component) != 1 || !component %in% units))
    stop("Unknown component: ", paste(component, collapse = ", "),
         call. = FALSE)
  # The empty set holds no date and NA is not known,
  # so there is nothing to annotate.
  if (.has_missing(x))
    return(.with_missing(x, annotate_component, component = component,
                         mark = mark))
  x <- as.character(x)
  vals <- unique(x)
  bit <- c("~" = 1L, "?" = 2L)[[mark]]
  out <- vapply(vals, .map_members, character(1), USE.NAMES = FALSE,
                f = function(tok) .tag_token(tok, component, bit))
  as_messydate(out[match(x, vals)])
}

# Applies `f` to each date in a value: each end of a range and each member
# of a set. The ".." of a range and the brackets of a set stay as they are.
.map_members <- function(v, f) {
  open <- stringi::stri_match_first_regex(v, "^([{\\[])(.*)([}\\]])$")
  if (!is.na(open[1, 1])) {
    members <- stringi::stri_split_fixed(open[1, 3], ",")[[1]]
    members <- vapply(members, .map_members, character(1), f = f,
                      USE.NAMES = FALSE)
    return(paste0(open[1, 2], paste(members, collapse = ","), open[1, 4]))
  }
  ends <- stringi::stri_split_fixed(v, "..")[[1]]
  ends[nzchar(ends)] <- vapply(ends[nzchar(ends)], f, character(1),
                               USE.NAMES = FALSE)
  paste(ends, collapse = "..")
}

.mark_chars <- c("~", "?", "%")
.mark_bits <- function(m) {
  out <- match(m, .mark_chars)
  out[is.na(out)] <- 0L
  out
}

# Tags one date or date-time. A mark state is kept for each component as
# bits: 1 is ~, 2 is ?, and 3 (both) is %. A mark before a component covers
# that component. A mark after a component covers that component and the
# components to its left.
.tag_token <- function(tok, component, bit) {
  bare <- grepl("^[~?%]?[0-9X]{1,2}:", tok)
  sep <- stringi::stri_extract_first_regex(tok, "[T ]")
  has_t <- !bare && !is.na(sep)
  date <- if (bare) "" else if (has_t) sub("[T ].*$", "", tok) else tok
  time <- if (bare) tok else if (has_t) sub("^[^T ]*[T ]", "", tok) else ""

  d <- NULL
  if (nzchar(date)) {
    m <- stringi::stri_match_first_regex(date, paste0(
      "^([~?%]?)(-?)([~?%]?)([0-9X]+)([~?%]?)",
      "(?:-([~?%]?)([0-9X]+)([~?%]?))?",
      "(?:-([~?%]?)([0-9X]+)([~?%]?))?$"))
    if (is.na(m[1, 1])) return(tok)
    lead <- c(paste0(m[1, 2], m[1, 4]), m[1, 7], m[1, 10])
    digits <- c(m[1, 5], m[1, 8], m[1, 11])
    trail <- c(m[1, 6], m[1, 9], m[1, 12])
    n <- sum(!is.na(digits))
    d <- list(sign = m[1, 3], digits = digits[seq_len(n)],
              state = .mark_bits(lead[seq_len(n)]))
    for (i in seq_len(n)) {
      tb <- .mark_bits(trail[i])
      if (tb > 0) d$state[seq_len(i)] <- bitwOr(d$state[seq_len(i)], tb)
    }
  }
  t <- NULL
  if (nzchar(time)) {
    off <- stringi::stri_extract_first_regex(time,
                                             "(Z|[+-][0-9]{2}:[0-9]{2})$")
    core <- sub("(Z|[+-][0-9]{2}:[0-9]{2})$", "", time)
    tail_mark <- .mark_bits(stringi::stri_extract_first_regex(core,
                                                              "[~?%]$"))
    core <- sub("[~?%]$", "", core)
    parts <- strsplit(core, ":", fixed = TRUE)[[1]]
    pm <- stringi::stri_match_first_regex(parts,
                                          "^([~?%]?)([0-9X]+(?:\\.[0-9]+)?)$")
    if (anyNA(pm[, 1])) return(tok)
    t <- list(digits = pm[, 3], state = .mark_bits(pm[, 2]),
              off = if (is.na(off)) "" else off)
    t$state <- bitwOr(t$state, tail_mark)
  }

  nd <- length(d$digits)
  nt <- length(t$digits)
  # An unspecified component ("XX") is not known, so it is not tagged as
  # approximate or uncertain; the tag goes on the components that are given.
  given <- which(!grepl("^X+$", d$digits))
  if (is.null(component)) {
    # The default tags the finest component that the value gives.
    target_d <- if (nt == 0 && length(given)) max(given) else integer(0)
    target_t <- if (nt > 0) nt else integer(0)
  } else {
    target_d <- switch(component, year = 1L, month = 2L, day = 3L,
                       md = 2:3, ym = 1:2, date = seq_len(nd),
                       integer(0))
    target_t <- switch(component, hour = 1L, minute = 2L, second = 3L,
                       time = seq_len(nt), integer(0))
  }
  target_d <- target_d[target_d <= nd & target_d %in% given]
  target_t <- target_t[target_t <= nt]
  if (length(target_d)) d$state[target_d] <- bitwOr(d$state[target_d], bit)
  if (length(target_t)) t$state[target_t] <- bitwOr(t$state[target_t], bit)

  out_d <- if (nd) .write_date(d, has_t) else ""
  out_t <- if (nt) .write_time(t) else ""
  if (bare) out_t else if (has_t) paste0(out_d, sep, out_t) else out_d
}

.mark_char <- function(b) ifelse(b > 0, .mark_chars[pmax(b, 1L)], "")

# Writes the date components with the fewest marks. One mark after
# component k covers components 1 to k when they all have the same mark.
# The parser does not read a mark after the day when a time follows,
# so then the mark goes after the month at the latest.
.write_date <- function(d, has_t) {
  n <- length(d$digits)
  s <- d$state
  k <- 0L
  if (s[1] > 0) {
    k <- 1L
    while (k < n && s[k + 1] == s[1]) k <- k + 1L
  }
  if (has_t && k == 3L) k <- 2L
  if (k < 2L) k <- 0L
  lead <- .mark_char(s)
  lead[seq_len(k)] <- ""
  trail <- rep("", n)
  if (k > 0) trail[k] <- .mark_char(s[k])
  parts <- paste0(lead, d$digits, trail)
  # A mark on a negative year goes before the sign.
  if (nzchar(d$sign)) {
    parts[1] <- paste0(lead[1], "-", d$digits[1], trail[1])
  }
  paste(parts, collapse = "-")
}

# Writes the time components. One mark at the end covers the whole time of
# day when all components have the same mark.
.write_time <- function(t) {
  s <- t$state
  if (all(s > 0) && length(unique(s)) == 1) {
    return(paste0(paste(t$digits, collapse = ":"), .mark_char(s[1]), t$off))
  }
  paste0(paste(paste0(.mark_char(s), t$digits), collapse = ":"), t$off)
}

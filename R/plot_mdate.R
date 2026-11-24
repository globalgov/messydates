#' Plot messy dates
#' @description
#'   `plot()` draws one line for each messy date, so that its imprecision can
#'   be seen before it is resolved.
#'   The height of each shape is the weight of each possible day:
#'
#'   - A precise date is a point.
#'   - A range, or a value with year or month precision, is a bar of full
#'     height, because each of its days has the same weight.
#'   - A set, or a value with unspecified digits (`X`), shows each of its
#'     possible dates as a point, joined by a dotted line.
#'   - An approximate value (`~` or `%`), with `approx_range` more than 0,
#'     is a trapezoid: a full-height core across the dates that the value
#'     names, and sides that fall to 0 over the dates that `approx_range`
#'     adds. These are the weights that `vrandom()` uses.
#'   - An uncertain value (`?` or `%`) has a dashed outline and no fill,
#'     because the source doubts the record, but its dates do not change.
#'
#'   Missing values and the empty set, `{}`, are not drawn.
#'   A time of day is not drawn; each value is drawn by its days.
#' @param x An `mdate` object.
#' @param labels A character vector of labels, one for each element of `x`.
#'   By default, the messy dates themselves.
#' @param approx_range The number of units that an approximate value widens
#'   by on each side, as in `possible_dates()`. By default 0, so that
#'   approximate values are drawn as if they were not approximate.
#' @param sort Should the lines be sorted by the earliest possible date?
#'   `TRUE` by default. If `FALSE`, the lines are in the order of `x`.
#' @param main A title for the plot.
#' @param ... Other arguments passed to `plot.default()`.
#' @return A data frame, invisibly, with the value, label, earliest day and
#'   latest day of each element that was drawn, in the order drawn from the
#'   top.
#' @name plot_mdate
#' @examples
#' plot(battles$Date, labels = battles$Battle)
#' plot(as_messydate(c("2012-03-15", "2012-04", "2012-05-10..2012-06-20",
#'                     "2012-07-~15", "2012-~09", "2012-10?",
#'                     "{2012-02-01,2012-11-15}", "2012-XX-28")),
#'      approx_range = 3)
#' @export
plot.mdate <- function(x, labels = NULL, approx_range = 0, sort = TRUE,
                       main = NULL, ...) {
  s <- as.character(x)
  if (is.null(labels)) labels <- s
  if (length(labels) != length(x))
    stop("`labels` must have one label for each element of `x`.",
         call. = FALSE)
  labels <- as.character(labels)
  ok <- !is.na(s) & !is_empty_notation(x)
  if (!any(ok)) stop("`x` has no dates to plot.", call. = FALSE)
  lo <- hi <- wlo <- whi <- rep(NA_real_, length(x))
  lo[ok] <- as.numeric(as.Date(vmin(x[ok]), FUN = identity))
  hi[ok] <- as.numeric(as.Date(vmax(x[ok]), FUN = identity))
  members <- vector("list", length(x))
  members[ok] <- lapply(possible_dates(x[ok]), .plot_days)
  # A value whose possible dates have gaps is drawn as separate points.
  gaps <- vapply(members, function(m) length(m) > 1 && any(diff(sort(m)) > 1),
                 logical(1))
  apx <- ok & approx_range > 0 & (date_is_approximate(x) %in% TRUE)
  if (any(apx)) {
    wide <- lapply(possible_dates(x[apx], approx_range = approx_range),
                   .plot_days)
    wlo[apx] <- vapply(wide, min, numeric(1))
    whi[apx] <- vapply(wide, max, numeric(1))
  }
  unc <- date_is_uncertain(x) %in% TRUE
  ord <- if (sort) order(lo, hi, na.last = NA) else which(ok)
  ord <- ord[ok[ord]]
  n <- length(ord)
  y <- rev(seq_len(n))
  xr <- range(c(lo, hi, wlo, whi, unlist(members)), na.rm = TRUE) + c(-1, 1)

  ink <- "#1f2937"; range_col <- "#4a76e8"; wide_col <- "#bccdf8"
  grid_col <- "#e5e7eb"; note_col <- "#6b7280"
  # The left margin fits the longest label; 0.065 inches is about the width
  # of one character at the label size used below.
  left <- min(max(nchar(labels[ord]), 4) * 0.065 + 0.3, 3.5)
  op <- graphics::par(mai = c(1.35, left, if (is.null(main)) 0.2 else 0.5, 0.2))
  on.exit(graphics::par(op))
  graphics::plot.default(NA, xlim = xr, ylim = c(0.5, n + 0.5), xaxt = "n",
                         yaxt = "n", bty = "n", xlab = "", ylab = "",
                         main = main, ...)
  # pretty() on dates puts the ticks on the first day of a month or a year.
  tick_dates <- tryCatch(pretty(structure(xr, class = "Date"), n = 8),
                         error = function(e) structure(pretty(xr, n = 8),
                                                       class = "Date"))
  ticks <- as.numeric(tick_dates)
  keep <- ticks >= xr[1] & ticks <= xr[2]
  ticks <- ticks[keep]
  tick_dates <- structure(ticks, class = "Date")
  fmt <- if (diff(xr) > 3 * 365) "%Y" else "%b %Y"
  graphics::abline(v = ticks, col = grid_col)
  graphics::axis(1, at = ticks, labels = format(tick_dates, fmt),
                 col = grid_col, col.axis = ink, cex.axis = 0.75)
  graphics::axis(2, at = y, labels = labels[ord], las = 1, tick = FALSE,
                 cex.axis = 0.75, col.axis = ink)

  h <- 0.32
  for (k in seq_len(n)) {
    i <- ord[k]
    yy <- y[k]
    a <- lo[i] - 0.5
    b <- hi[i] + 0.5
    if (apx[i] && !gaps[i]) {
      wa <- min(wlo[i] - 0.5, a)
      wb <- max(whi[i] + 0.5, b)
      graphics::polygon(c(wa, a, b, wb, b, a),
                        c(yy, yy + h, yy + h, yy, yy - h, yy - h),
                        col = wide_col, border = range_col, lwd = 0.8)
    }
    if (gaps[i]) {
      m <- members[[i]]
      graphics::segments(min(m), yy, max(m), yy, lty = 3, col = range_col)
      graphics::points(m, rep(yy, length(m)), pch = 19, cex = 0.8,
                       col = range_col)
    } else if (lo[i] == hi[i]) {
      if (apx[i])
        graphics::segments(lo[i], yy - h, lo[i], yy + h, col = range_col,
                           lwd = 2)
      graphics::points(lo[i], yy, pch = if (unc[i]) 21 else 19, bg = "white",
                       cex = 0.8, col = ink)
    } else {
      graphics::rect(a, yy - h, b, yy + h,
                     border = if (unc[i]) range_col else NA,
                     col = if (unc[i]) "white" else range_col,
                     lty = if (unc[i]) 2 else 1)
    }
  }

  show_apx <- any(apx)
  below <- graphics::grconvertY(graphics::grconvertY(graphics::par("usr")[3],
                                                     "user", "inches") - 0.55,
                                "inches", "user")
  graphics::legend(x = mean(graphics::par("usr")[1:2]), y = below,
                   xjust = 0.5, yjust = 1, xpd = NA, ncol = 3, bty = "n",
                   cex = 0.65,
                   text.width = diff(graphics::par("usr")[1:2]) / 5,
                   legend = c("precise", "range or imprecise", "set member",
                              "uncertain (?)",
                              if (show_apx) "approximate: weight falls to 0"),
                   pch = c(19, NA, 19, 21, if (show_apx) NA),
                   fill = c(NA, range_col, NA, NA, if (show_apx) wide_col),
                   border = c(NA, NA, NA, NA, if (show_apx) range_col),
                   col = c(ink, NA, range_col, ink, if (show_apx) NA))
  graphics::mtext("The height of each shape is the weight of each day.",
                  side = 1, line = 5.3, cex = 0.6, col = note_col)
  invisible(data.frame(value = s[ord], label = labels[ord],
                       min = structure(lo[ord], class = "Date"),
                       max = structure(hi[ord], class = "Date")))
}

# The days of a vector of possible dates, as numbers of days since
# 1970-01-01. A time of day is dropped, and BCE dates are kept.
.plot_days <- function(d) {
  d <- d[!is.na(d)]
  if (!length(d)) return(numeric(0))
  as.numeric(as.Date(new_messydate(strip_times(d)), FUN = identity))
}

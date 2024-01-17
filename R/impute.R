#' @examples
#'   lapply(impute(battles, 3), function(x) mean(x$Date))
#' @export
impute <- function(.data, times = 1000) {
  lapply(seq.int(times), function(x){
    .data |> dplyr::mutate(dplyr::across(dplyr::where(is_messydate), random))
  })
}


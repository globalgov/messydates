#' @examples
#'   lapply(impute(battles, 3), function(x) mean(x$Date))
#' @export
impute <- function(.data, times = 1000) {
  lapply(seq.int(times), function(x){
    .data |> dplyr::mutate(dplyr::across(dplyr::where(is_messydate), random))
  })
}

#' @examples
#' library(psfmi)
#' res <- to_pool(impute(battles, 10)) |>
#'   dplyr::mutate(Date = as.numeric(Date)) |>
#'   psfmi_coxr(Surv(Date, status) ~ US_party + N_actors, impvar = "impute_num")
#' res$RR_model_final
#' @export
to_pool <- function(imputes) {
  impute_num <- NULL
  dplyr::bind_rows(imputes, .id = "impute_num") |>
    dplyr::mutate(impute_num = as.numeric(impute_num),
                  status = 1)
}

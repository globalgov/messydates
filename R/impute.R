#' Imputation for modelling using mdates
#'
#' @name impute
#' @param .data A dataset with mdate variables.
#' @param times Numebr of times for imputation.
#' @param imputes Imputed data.
#' @import dplyr
#' @examples
#' lapply(impute(battles, 3), function(x) mean(x$Date))
#' #library(psfmi)
#' #res <- to_pool(impute(battles, 10)) |>
#' #   dplyr::mutate(Date = as.numeric(Date)) |>
#' #   psfmi_coxr(Surv(Date, status) ~ US_party + N_actors, impvar = "impute_num")
#' #res$RR_model_final
NULL

#' @rdname impute
#' @export
impute <- function(.data, times = 1000) {
  lapply(seq.int(times), function(x){
    .data |> dplyr::mutate(dplyr::across(dplyr::where(is_messydate), random))
  })
}

#' @rdname impute
#' @export
to_pool <- function(imputes) {
  impute_num <- NULL
  dplyr::bind_rows(imputes, .id = "impute_num") |>
    dplyr::mutate(impute_num = as.numeric(impute_num),
                  status = 1)
}

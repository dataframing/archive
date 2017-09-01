#' Convert a copy + pasted BigQuery output table into a tibble for sharing.
#'
#' I'm not proud of this function, but I'd be lying if I said I didn't use it.
#' To use it, first run any query in BigQuery. Once the results are returned,
#' you can just copy and paste the table output into a script or console, surround it with
#' quotes, and pipe it into this function.
#'
#' @param table.text the copied text output from a BQ query
#' @export
read.bq.table <- function(table.text) {
  table.text %>%
    utils::read.table(text = ., sep = "\t", header = T) %>%
    dplyr::select(-dplyr::one_of('Row', 'X')) %>%
    tibble::as.tibble()
}

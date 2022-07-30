#' Get max length of ID data.
#'
#' @param data A dataframe.
#' @importFrom dplyr pull
#' @return A int.
#' @export
mt_get_padlen <- function(data){
  data %>%
  pull("ID") %>%
  map(nchar) %>%
  unlist() %>%
  max()
}
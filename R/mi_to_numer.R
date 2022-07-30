#' Convert data to numeric, and for ID column convert with fixed levels.
#' @export
#' @importFrom  dplyr across mutate
#' @param data A tibble with n position column(pos1,pos2,...) and class column.
#' @param levels Characters accommodated in IDs.
#' @examples
#' data(mi_data_rawID)
#' str(mi_split_col(mi_data_rawID,cores = 1,pad_len = 10))
#' str(mi_to_numer(mi_split_col(mi_data_rawID,cores = 1,pad_len = 10)))
mi_to_numer <- function(data, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":")) {
  data %>%
    mutate(across(.cols = -"class", .fns = ~ factor(.x, levels = levels))) %>%
    mutate(across(.cols = "class", .fns = factor)) %>%
    mutate(across(.cols = -"class", .fns = as.numeric))
}
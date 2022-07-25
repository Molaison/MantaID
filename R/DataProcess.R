#' Reshape data and delete meaningless rows.
#'
#' @param data A dataframe or tibble or data.table or matrix. Names of the column will be regard as the class of ID included in column.
#' @param placeholder Character vectors. IDs included in `placeholder` will be omitted.
#' @param cols Character vectors. Columns of `data` that contain the IDs
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate_at pull slice
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom magrittr %<>%
#' @export
#' @return A tibble with two columns("ID" and "class")
#' @examples
#' data <- tibble::tibble(
#' "class1" = c("A","B","C","D"),
#' 'class2' = c("E","F","G","H"),
#' 'class3' = c("L","M","-","O")
#' )
#' data_clean(data)
data_clean <- function(data, cols = everything(), placeholder = c("-")) {
  data %<>% as_tibble() %>%
    select(cols) %>%
    mutate_at(colnames(.), ~ as.character(.x)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "class",
      values_to = "ID",
      names_repair = "minimal"
    ) %>%
    select("ID", "class")
  index <- map(data %>% pull("ID"), ~ !.x %in% placeholder)
  data %>% slice(which(index == TRUE))
}


#' Split the string into individual characters and complete the character vector to the maximum length.
#'
#' @param str The string to be splited.
#' @param pad_len The length of longest id, i.e. the maxlength.
#' @export
#' @return Splited character vector.
#' @examples
#' string_test = "Good Job"
#' length = 15
#' string_split(string_test,length)
string_split <- function(str, pad_len) {
  str %>%
    as.character() %>%
    strsplit(split = "") %>%
    unlist() %>%
    c(., rep("*", pad_len - length(.)))
}


#' Cut the string of ID column character by character and divide it into multiple columns.
#'
#' @param data Dataframe(tibble) to be split.
#' @param cores Int.The num of cores to allocate for computing.
#' @param pad_len The length of longest id, i.e. the maxlength.
#' @importFrom parallel detectCores makeCluster clusterExport clusterEvalQ parSapply stopCluster
#' @importFrom  dplyr bind_cols
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#' @return A tibble with pad_len+1 column.
#' @export
#' @examples
#' data(ID_sample)
#' col_split(ID_sample,cores = 1,pad_len = 10)
col_split <- function(data, cores = NULL, pad_len = 10) {
  # 2.计算计算机内核数
  core_max <- detectCores(logical = FALSE)
  # 3.打开并行计算
  if (is.null(cores)) {
    cl <- makeCluster(core_max)
  } else {
    cl <- makeCluster(cores)
  }
  string_split <- function(str, pad_len) {
    str %>%
      as.character() %>%
      strsplit(split = "") %>%
      unlist() %>%
      c(., rep("*", ifelse((pad_len - length(.)) > 0, pad_len - length(.), 0))) %>%
      .[1:pad_len]
  }
  # 4.给每个单独内核传递变量,函数等
  clusterExport(cl, varlist = c("pad_len", "string_split"), envir = environment())
  clusterEvalQ(cl, c(library(data.table), library(magrittr), library(stringr),library(dplyr)))
  # 5.开始并行计算（用法与sapply类似）
  output <- parSapply(cl, data[, 1][[1]], string_split, pad_len)
  # 6.关闭并行计算
  stopCluster(cl)
  output %>%
    unlist() %>%
    matrix(byrow = TRUE, ncol = pad_len) %>%
    bind_cols(data[, 2]) %>%
    set_names(c(str_c("pos", 1:pad_len),"class"))
}
#' Convert data to factor type, and for ID column convert with fixed levels.
#' @export
#' @importFrom  dplyr across mutate
#' @param data A tibble with n position column(pos1,pos2,...) and class column.
#' @param levels Characters accommodated in IDs.
#' data(ID_sample)
#' str(col_split(ID_sample,cores = 1,pad_len = 10))
#' str(to_factor(col_split(ID_sample,cores = 1,pad_len = 10)))
to_factor <- function(data, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":")) {
  data %>%
    mutate(across(.cols = -"class", .fns = ~ factor(.x, levels = levels))) %>%
    mutate(across(.cols = "class", .fns = factor))
}

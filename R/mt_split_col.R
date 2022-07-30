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
#' data(mt_data_rawID)
#' mt_split_col(mt_data_rawID,cores = 1,pad_len = 10)
mt_split_col <- function(data, cores = NULL, pad_len = 10) {
  # 2.计算计算机内核数
  core_max <- detectCores(logical = FALSE)%/%2
  # 3.打开并行计算
  if (is.null(cores)) {
    cl <- makeCluster(core_max)
  } else {
    cl <- makeCluster(cores)
  }
  mt_split_str <- function(str, pad_len) {
    str %>%
      as.character() %>%
      strsplit(split = "") %>%
      unlist() %>%
      c(., rep("*", ifelse((pad_len - length(.)) > 0, pad_len - length(.), 0))) %>%
      .[1:pad_len]
  }
  # 4.给每个单独内核传递变量,函数等
  clusterExport(cl, varlist = c("pad_len", "mt_split_str"), envir = environment())
  clusterEvalQ(cl, c(library(data.table), library(magrittr), library(stringr),library(dplyr)))
  # 5.开始并行计算（用法与sapply类似）
  output <- parSapply(cl, data[, 1][[1]], mt_split_str, pad_len)
  # 6.关闭并行计算
  stopCluster(cl)
  output %>%
    unlist() %>%
    matrix(byrow = TRUE, ncol = pad_len) %>%
    bind_cols(data[, 2]) %>%
    set_names(c(str_c("pos", 1:pad_len),"class"))
}
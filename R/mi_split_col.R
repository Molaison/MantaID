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
mi_split_col <- function(data, cores = NULL, pad_len = 10) {
  #Control the number of CPU cores.
  core_max <- detectCores(logical = FALSE) %/% 2
  #Allocate cores.
  if (is.null(cores)) {
    cl <- makeCluster(core_max, port = 1223)
  } else {
    cl <- makeCluster(cores, port = 1223)
  }
  #Cut a single ID character and convert it to a vector.
  mi_split_str <- function(str, pad_len) {
    str %>%
      as.character() %>%
      strsplit(split = "") %>%
      unlist() %>%
      #Determine the length of ID characters, select the maximum character length, and use "*" to fill in the missing positions of all ID characters.
      c(., rep("*", ifelse((pad_len - length(.)) > 0, pad_len - length(.), 0))) %>%
      .[1:pad_len]
  }
  #Input variables "pad_len" and "mi_split_str".
  clusterExport(cl, varlist = c("pad_len", "mi_split_str"), envir = environment())
  #Load packages, and reload packages that will be used in subsequent scripts running on multiple cores.
  clusterEvalQ(cl, c(library(magrittr)))
  #Parallel Computing.
  output <- parSapply(cl, data[, 1][[1]], mi_split_str, pad_len)
  #Stop the cluster
  stopCluster(cl)
  #Output final result and reset column names.
  output %>%
    unlist() %>%
    matrix(byrow = TRUE, ncol = pad_len) %>%
    bind_cols(data[, 2]) %>%
    set_names(c(str_c("pos", 1:pad_len), "class"))
}

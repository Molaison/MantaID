#' Data balance.
#' Most classes adopt random undersampling, while a few classes adopt smote method to oversample to obtain relatively balanced data;
#'
#' @param data A data frame. Except class column, all are numeric types.
#' @param ratio Numeric between 0 and 1. The percent of test set split from data.
#' @param parallel Logical.
#'
#' @return A list contain train set and test set.
#' @export
#' @importFrom scutr SCUT_parallel SCUT oversample_smote resample_random
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif partition
#' @importFrom dplyr slice bind_rows
#' @importFrom magrittr set_names
#' @examples
#' library(dplyr)
#' data = rename(iris,class =Species)
#' balance_data(data)
balance_data <- function(data, ratio = 0.3, parallel = F) {
  system.time({
    # 多分类数据平衡,欠采样样本采用随机采样,过采样部分采用smote方法采样,获得相对平衡的数据;
    if(parallel){
      data_smtd <- SCUT_parallel(data, "class", oversample = oversample_smote, undersample = resample_random)
    }else{
      data_smtd <- SCUT(data, "class", oversample = oversample_smote, undersample = resample_random)
    }
  })
  #差分平衡后数据集与平衡前数据集,作为训练集
  train_new <- setdiff(data_smtd, data)
  print(train_new %>% nrow())
  task <- data %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -"class")
  # 原数据集中分离出一部分同样作为训练集
  train_raw <-partition(task, ratio = 1-ratio)$train %>% slice(data, .)
  #训练集数据是原来数据集的拆分加上采样获得的新样本
  train <- bind_rows(train_raw, train_new)
  # 测试集数据来自原来数据集的拆分
  test <-partition(task, ratio = 1-ratio)$test %>% slice(data, .)

  return(list(train, test))
}

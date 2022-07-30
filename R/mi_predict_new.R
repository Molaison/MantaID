#' Predict new data with trained learner.
#' @param data A dataframe.
#'
#' @param learner A R6 class object.
#' @importFrom dplyr rename mutate across select bind_cols
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @export
mi_predict_new <- function(data, learner) {
  data %>%
    mutate(across(.cols = everything(), .fns = as.numeric)) %>%
    # 将数据类型转为数值型
    learner$predict_newdata(newdata = ., task = NULL) # %>%
  # 调用学习器进行预测
  as.data.table() %>%
    # 将预测结果与数据组合
    bind_cols(data, .) %>%
    # 预测新数据,真实值未知
    select(-"truth") %>%
    # 将预测结果放到第一列
    select("response", everything()) %>%
    rename("response" = learner$id)
}
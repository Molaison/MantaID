globalVariables(".")
#' Predict new data with trained learner.
#' @param data A dataframe.
#'
#' @param learner A R6 class object.
#' @importFrom dplyr rename mutate across select bind_cols
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @export
mt_predict_new <- function(data, learner) {
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

#' Observe the distribution of the false response of test set.
#' @param predict A R6 class `PredictionClassif`.
#' @importFrom data.table as.data.table
#' @importFrom dplyr filter group_by count
#' @importFrom magrittr %>% %T>%
#' @export
#' @return A tibble data frame that records the number of wrong predictions for each category ID;
mt_get_miss <- function(predict) {
  stopifnot(any(class(predict) == "PredictionClassif"))
  result <- predict %>%
    as.data.table() %>%
    filter("truth" != "response") %>%
    group_by("truth") %>%
    count() %T>%
    print()
}

#' Compute the confusion matrix for the predict result.
#'
#' @param ifnet Logical.Whether the data is obtained by a deep learning model.
#' @param result_list A list return from model training functions.
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr select
#' @importFrom caret confusionMatrix
#' @return A `confusionMatrix` object.
#' @export
#'
mt_get_confusion <- function(result_list, ifnet = F) {
  if (ifnet) {
    return(result_list[[2]])
  }
  matri_tr <- result_list[[2]] %>%
    as.data.table() %>%
    select(-1)
  confusionMatrix(matri_tr %>% pull(2), matri_tr %>% pull(1))
}
#' Plot heatmap for result confusion matrix.
#'
#' @param table A table.
#' @param filepath File path the plot to save.Default NULL.
#' @param name Model names.
#'
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate group_by ungroup across na_if
#' @importFrom ggplot2 coord_equal ggplot aes geom_text geom_tile theme_bw coord_equal scale_fill_gradient2 labs theme element_text ggsave
#' @importFrom stringr str_c
#' @return A `ggplot` object.
#' @export

mt_plot_heatmap <- function(table,name = NULL, filepath = NULL) {
  melted <- table %>%
    as.table() %>%
    melt() #%>%
    # group_by(.data[['Reference']]) %>%
    # mutate(across(.cols = .data[['value']], .fns = ~ .x / sum(.x))) %>%
    # ungroup()

  heat <- ggplot(melted, aes(x = .data[['Reference']], y =.data[['Prediction']], fill = .data[['value']])) +
    geom_tile() +
    geom_text(aes(label = na_if(round(.data[['value']], 2), 0)), color = "#00468B99", size = 2) +
    theme_bw() +
    coord_equal() +
    # scale_fill_gradient2(low="#003366", high="#990033",mid = "white") +
    scale_fill_gradient2(name = NULL, high = "#ED000099", mid = "white") +
    labs(title = "Confusion Matrix") +
    theme(
      axis.text.x = element_text(
        angle = 45,
        vjust = 1, size = 10, hjust = 1,
        lineheight = 10
      ),
      axis.text.y = element_text(size = 10),
      strip.text.y = element_text(
        angle = 0,
        vjust = 0.5,
        hjust = 0.5,
        size = 10
      )
    )
  if (!is.null(filepath)) {
    ggsave(str_c(filepath, "heatmap_", name, ".png"), width = 12.37, height = 10, plot = heat, device = "png", dpi = 600)
  }
  return(heat)
}

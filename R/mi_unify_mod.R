#' Predict with four models and unify results by the sub-model's specificity score to the four possible classes.
#'
#' @param data A dataframe contains the ID column.
#' @param col_id The name of ID column.
#' @importFrom dplyr rename select mutate across bind_cols  summarise group_by
#' @importFrom data.table as.data.table
#' @importFrom keras k_argmax
#' @return A dataframe.
#' @export
mi_unify_mod <- function(data, col_id, result_rg, result_rp, result_xgb, result_BP, c_value = 0.75, pad_len = 30) {
  data <- data %>%
    select(col_id, everything()) %>%
    rename("ID" = col_id) %>%
    mutate(across(.cols = "ID", .fns = as.character))
  string_split <- function(str, pad_len) {
    str %>%
      as.character() %>%
      strsplit(split = "") %>%
      unlist() %>%
      c(., rep("*", ifelse((pad_len - length(.)) > 0, pad_len - length(.), 0))) %>%
      .[1:pad_len] %>%
      set_names(str_c("pos", 1:pad_len))
  }
  prd_new <- function(data, learner) {
    data %>%
      mutate(across(.col = everything(), .fns = as.numeric))
    # 将数据类型转为数值型

    learner$predict_newdata(data) %>%
      # 调用学习器进行预测
      as.data.table() %>%
      # 将预测结果与数据组合
      bind_cols(data, .) %>%
      # #预测新数据，真实值未知
      select(-truth, -row_ids) %>%
      # #将预测结果放到第一列
      select(response, everything())
  }
  major <- function(Deeplearn, DecisionTree, RandomForest, Xgboost) {
    vec <- c(Deeplearn, DecisionTree, RandomForest, Xgboost)
    tab <- vec %>%
      table() %>%
      reshape2::melt()
    xtab <- table(vec)
    xmode <- names(which(xtab == max(xtab)))
    # return(length(xmode))

    if (length(xmode) == 1) {
      sprintf("xmode = %d", length(xmode))
      return(xmode)
    } else if (length(xmode) == 2) {
      sprintf("xmode = %d", length(xmode))

      result <- tibble(
        names = vec,
        weight = c(
          final(vec = swap(vec, 1), conf_list = swap(conf_list, 1), c = c_value),
          final(vec = swap(vec, 2), conf_list = swap(conf_list, 2), c = c_value),
          final(vec = swap(vec, 3), conf_list = swap(conf_list, 3), c = c_value),
          final(vec = swap(vec, 4), conf_list = swap(conf_list, 4), c = c_value)
        )
      ) %>%
        group_by(names) %>%
        summarise(sum = sum(weight))
      return(as.character(result[[which.max(result$sum), 1]]))
    } else if (length(xmode) == 4) {
      sprintf("xmode = %d", length(xmode))
      result <- c(
        final(vec = swap(vec, 1), conf_list = swap(conf_list, 1)),
        final(vec = swap(vec, 2), conf_list = swap(conf_list, 2)),
        final(vec = swap(vec, 3), conf_list = swap(conf_list, 3)),
        final(vec = swap(vec, 4), conf_list = swap(conf_list, 4))
      )
      return(as.character(vec[which.max(result)]))
    }
  }

  swap <- function(lst, i) {
    temp <- lst[[i]]
    lst[[i]] <- lst[[1]]
    lst[[1]] <- temp
    return(lst)
  }

  final <- function(vec, conf_list, c = 0.75) {
    vec <- vec %>% as.character()
    truth <- vec[1]
    value <- conf_list[[1]][truth, truth] / sum(conf_list[[1]][, truth])

    for (i in 2:4) {
      value <- value * (conf_list[[i]][truth, vec[i]] / sum(conf_list[[i]][, vec[i]]) * (1 - c) + c)
    }
    return(value)
  }
  result <- map_dfr(pull(data, "ID"), string_split, pad_len) %>%
    mutate(across(.cols = everything(), .fns = ~ factor(.x, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":")))) %>%
    mutate(across(.cols = everything(), .fns = ~ as.numeric(.x)))

  learner_xgb <- result_xgb[[1]]
  learner_rp <- result_rp[[1]]
  learner_rg <- result_rg[[1]]
  learner_BP <- result_BP[[1]]
  conf_rp <- result_rp[[2]][["confusion"]]
  conf_rg <- result_rg[[2]][["confusion"]]
  conf_xgb <- result_xgb[[2]][["confusion"]]
  conf_net <- result_BP[[2]][["table"]]
  conf_list <- list(conf_net, conf_rp, conf_rg, conf_xgb)
  rp <- prd_new(as.data.table(result), result_rp[[1]])
  rg <- prd_new(result, result_rg[[1]])
  xgb <- prd_new(result, result_xgb[[1]])

  predictions <- predict(learner_BP, as.matrix(result))
  response <- predictions %>% k_argmax()
  response <- response$numpy() %>%
    as.numeric(.)
  level_ <- result_BP[[3]]
  response <- level_[response + 1] %>% factor(level_)

  predict_all <- response %>%
    bind_cols(select(rp, "response")) %>%
    bind_cols(select(rg, "response")) %>%
    bind_cols(select(xgb, "response")) %>%
    set_names(c("Deeplearn", "DecisionTree", "RandomForest", "Xgboost"))
  major_result <- pmap(predict_all, major) %>%
    unlist() %>%
    bind_cols(predict_all) %>%
    set_names(c("Integrated", "Deeplearn", "DecisionTree", "RandomForest", "Xgboost"))

  return(major_result)
}

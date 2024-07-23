#' Predict with four models and unify results by the sub-model's specificity score to the four possible classes.
#'
#' @param data A dataframe contains the ID column.
#' @param col_id The name of ID column.
#' @param result_rg The result from the Random Forest model.
#' @param result_rp The result from the Decision Tree model.
#' @param result_xgb The result from the XGBoost model.
#' @param result_BP The result from the Backpropagation Neural Network model.
#' @param c_value A numeric value used in the final prediction calculation.
#' @param pad_len The length to pad the ID characters to.
#' @importFrom dplyr rename select mutate across bind_cols  summarise group_by
#' @importFrom data.table as.data.table
#' @importFrom keras k_argmax
#' @return A dataframe.
#' @export
mi_unify_mod <- function(data, col_id, result_rg, result_rp, result_xgb, result_BP, c_value = 0.75, pad_len = 30) {
#Select the col_id column from the data and rename it to ID, then convert the ID class to a character type.
  data <- data %>%
    select(col_id, everything()) %>%
    rename("ID" = col_id) %>%
    mutate(across(.cols = "ID", .fns = as.character))
  string_split <- function(str, pad_len) {
  #Cut a single ID character and convert it to a vector.
    str %>%
      as.character() %>%
      strsplit(split = "") %>%
      unlist() %>%
      #Determine the length of ID characters, select the maximum character length, and use "*" to fill in the missing positions of all ID characters.
      c(., rep("*", ifelse((pad_len - length(.)) > 0, pad_len - length(.), 0))) %>%
      .[1:pad_len] %>%
      set_names(str_c("pos", 1:pad_len))
  }
  #Converting data to numeric types.
  prd_new <- function(data, learner) {
    data %>%
      mutate(across(.cols = everything(), .fns = as.numeric))
    #Call the learner to make predictions, transform the results into a table, combine the predictions with the data, select columns other than the true value and the original ID, and put the predictions in the first column.
    learner$predict_newdata(data) %>%
      as.data.table() %>%
      bind_cols(data, .) %>%
      select(-truth, -row_ids) %>%
      select(response, everything())
  }
  major <- function(BPNN, DT, RF, XGB) {
    #Convert to vector.
    vec <- c(BPNN, DT, RF, XGB)
    #Convert the vector into a table form and then combine multiple columns into a single column.
    tab <- vec %>%
      table() %>%
      reshape2::melt()
    #Convert to table form.
    xtab <- table(vec)
    #return length(xmode).
    xmode <- names(which(xtab == max(xtab)))
    #When length(xmode) is equal to 1 or 2, the corresponding length(xmode) is output in the form of a decimal integer.
    if (length(xmode) == 1) {
      sprintf("xmode = %d", length(xmode))
      return(xmode)
    } else if (length(xmode) == 2) {
      sprintf("xmode = %d", length(xmode))
      #Build tibble data frames, group by name and count.
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
      #Convert the value at the specified location to character form.
      return(as.character(result[[which.max(result$sum), 1]]))
      #When length(xmode) is equal to 4, the corresponding length(xmode) is output in the form of a decimal integer.
    } else if (length(xmode) == 4) {
      sprintf("xmode = %d", length(xmode))
      result <- c(
        final(vec = swap(vec, 1), conf_list = swap(conf_list, 1)),
        final(vec = swap(vec, 2), conf_list = swap(conf_list, 2)),
        final(vec = swap(vec, 3), conf_list = swap(conf_list, 3)),
        final(vec = swap(vec, 4), conf_list = swap(conf_list, 4))
      )
      #Convert the value at the specified location to character form.
      return(as.character(vec[which.max(result)]))
    }
  }
  #Replace the value of the first column with the value of column i.
  swap <- function(lst, i) {
    temp <- lst[[i]]
    lst[[i]] <- lst[[1]]
    lst[[1]] <- temp
    return(lst)
  }

  final <- function(vec, conf_list, c = 0.75) {
    #Replace vec with the character form.
    vec <- vec %>% as.character()
    #Define teuth.
    truth <- vec[1]
    #Define value.
    value <- conf_list[[1]][truth, truth] / sum(conf_list[[1]][, truth])
    #For i equal to 2:4, redefine value.
    for (i in 2:4) {
      value <- value * (conf_list[[i]][truth, vec[i]] / sum(conf_list[[i]][, vec[i]]) * (1 - c) + c)
    }
    return(value)
  }
  #Convert data to numeric.
  result <- map_dfr(pull(data, "ID"), string_split, pad_len) %>%
    mutate(across(.cols = everything(), .fns = ~ factor(.x, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":")))) %>%
    mutate(across(.cols = everything(), .fns = ~ as.numeric(.x)))
  #Define the models corresponding to different learners.
  learner_xgb <- result_xgb[[1]]
  learner_rp <- result_rp[[1]]
  learner_rg <- result_rg[[1]]
  learner_BP <- result_BP[[1]]
  #Define the confusion matrix corresponding to different learners.
  conf_rp <- result_rp[[2]][["confusion"]]
  conf_rg <- result_rg[[2]][["confusion"]]
  conf_xgb <- result_xgb[[2]][["confusion"]]
  conf_net <- result_BP[[2]][["table"]]
  #Assemble the four confusion matrices into a list.
  conf_list <- list(conf_net, conf_rp, conf_rg, conf_xgb)
  #Converting data to numeric types.
  rp <- prd_new(as.data.table(result), result_rp[[1]])
  rg <- prd_new(result, result_rg[[1]])
  xgb <- prd_new(result, result_xgb[[1]])
  #Predict model.
  predictions <- predict(learner_BP, as.matrix(result))
  #Return the index of the maximum value.
  response <- predictions %>% k_argmax()
  #Create multi-dimensional arrays and convert them to numeric types.
  response <- response$numpy() %>%
    as.numeric(.)
  #To get the levels.
  level_ <- result_BP[[3]]
  #Modify level and convert to factor type.
  response <- level_[response + 1] %>% factor(level_)
  #Assemble results by column and name each column.
  predict_all <- response %>%
    bind_cols(select(rp, "response")) %>%
    bind_cols(select(rg, "response")) %>%
    bind_cols(select(xgb, "response")) %>%
    set_names(c("BPNN", "DT", "RF", "XGB"))
  #Apply the major function to each group of elements of predict_all, iterate, cancel the list form, convert levels to factor types, assemble predict_all by columns, and name each column.
  major_result <- pmap(predict_all, major) %>%
    unlist() %>%
    factor(levels = level_) %>%
    bind_cols(predict_all) %>%
    set_names(c("Integrated", "BPNN", "DT", "RF", "XGB"))
  return(major_result)
}

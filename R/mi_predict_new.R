#' Predict new data with a trained learner.
#' @param data A dataframe.
#'
#' @param learner An R6 class object.
#' @importFrom dplyr rename mutate across select bind_cols
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @return A data frame that contains features and 'predict' class.
#' @export
mi_predict_new <- function(data, result,ifnet = F) {
  if(ifnet){
    learner_BP = result[[1]]
    predictions <- predict(learner_BP, as.matrix(data))
    response <- predictions %>% k_argmax()
    response <- response$numpy() %>%
      as.numeric(.)
    level_ <- result[[3]]
    response <- level_[response + 1] %>% factor(level_)
    return(data %>% mutate("response" = response))
  }else{
    learner = result[[1]]
    data %>%
      mutate(across(.cols = everything(), .fns = as.numeric)) %>%
      learner$predict_newdata(newdata = ., task = NULL) # %>%
    as.data.table() %>%
      bind_cols(data, .) %>%
      select(-"truth") %>%
      select("response", everything()) %>%
      rename("response" = learner$id)
  }
}

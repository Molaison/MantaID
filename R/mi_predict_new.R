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
  #If data obtained by a deep learning model exists.
  if(ifnet){
    #select model.
    learner_BP = result[[1]]
    #Predict model.
    predictions <- predict(learner_BP, as.matrix(data))
    #Return the index of the maximum value.
    response <- predictions %>% k_argmax()
    #Create multi-dimensional arrays and convert them to numeric types.
    response <- response$numpy() %>%
      as.numeric(.)
    #select levels.
    level_ <- result[[3]]
    #Modify level and convert to factor type.
    response <- level_[response + 1] %>% factor(level_)
    #Return a data frame that contains features and 'predict' class.
    return(data %>% mutate("response" = response))
  #If data obtained by a deep learning model does not exist.
  }else{
    #select model.
    learner = result[[1]]
    #Predict model.
    data %>%
      mutate(across(.cols = everything(), .fns = as.numeric)) %>%
      learner$predict_newdata(newdata = ., task = NULL) # %>%
    #get a data frame that contains features and 'predict' class.
    as.data.table() %>%
      bind_cols(data, .) %>%
      select(-"truth") %>%
      select("response", everything()) %>%
      rename("response" = learner$id)
  }
}

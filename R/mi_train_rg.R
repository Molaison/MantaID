#' Random Forest Model Training.
#'
#' @param measure Model evaluation method.
#' @param train A dataframe.
#' @param test A dataframe.
#' @param instance A tuner.
#'
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @return  A list of learner for predicting and predicted result of test set.
#' @export
mi_train_rg <- function(train, test, measure = msr("classif.acc"), instance = NULL) {
  #Initialize the random forest learner and set the value of the hyperparameters.
  learner <- lrn("classif.ranger",
    importance = "impurity",
    regularization.factor = 0.01407, minprop = 0.01667, num.trees = 385,
    max.depth = 368
  )
  #When the result of the parameter adjustment exists, set the value of the parameters to the value of the hyperparameters at this time.
  if (!is.null(instance)) {
    learner$param_set$values <- instance$result_learner_param_vals
  }
  #Convert the training set into a table, and later convert it into a classification task.
  task_train <- train %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"), id = "train_rg")
  #Convert the test set to a table and later convert it to a classification task.
  task_predict <- test %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"), id = "test_rg")
  #Divide the training set.
  train_set <- partition(task_train, ratio = 1)$train
  #Divide the test set.
  test_set <- partition(task_predict, ratio = 0)$test
  #Set the Number of Threads.
  #set_threads(learner)
  #Train model.
  learner$train(task_train, row_ids = train_set)
  #Predict model.
  predict <- learner$predict(task_predict, row_ids = test_set)
  #Output the learner for predicting and predicted result as a list.
  list(learner, predict)
}

#' Classification tree model training.
#'
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param train A dataframe.
#' @param test A dataframe.
#' @param instance A tuner.
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @return A list of learner for predicting and predicted result of test set.
#' @export
mi_train_rp <- function(train, test, measure = msr("classif.acc"), instance = NULL) {
  #Initialize the decision tree learner and set the value of the hyperparameters.
  learner <- lrn("classif.rpart",
    keep_model = TRUE, xval = 0, cp = 0.0005253, maxdepth = 24,
    minsplit = 4, maxcompete = 3
  )
  #When the result of the parameter adjustment exists, set the value of the parameters to the value of the hyperparameters at this time.
  if (!is.null(instance)) {
    learner$param_set$values <- instance$result_learner_param_vals
  }
  #Convert the training set into a table, and later convert it into a classification task.
  task_train <- train %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"), id = "train_rp")
  #Convert the test set to a table and later convert it to a classification task.
  task_predict <- test %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"), id = "test_rp")
  #Divide the training set.
  train_set <- partition(task_train, ratio = 1)$train
  #Divide the test set.
  test_set <- partition(task_predict, ratio = 0)$test
  #Set the Number of Threads.
  set_threads(learner)
  #Train model.
  learner$train(task_train, row_ids = train_set)
  #Predict model.
  predict <- learner$predict(task_predict, row_ids = test_set)
  #Output the results of training and prediction as a list.
  list(learner, predict)
}

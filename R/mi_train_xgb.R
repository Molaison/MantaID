#' Xgboost model training
#'
#' @param measure Model evaluation method.
#' @param train A dataframe.
#' @param test A dataframe.
#' @param instance A tuner.
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @export
mi_train_xgb <- function(train, test, measure = msr("classif.acc"),instance = NULL) {
  learner <- lrn("classif.xgboost",nrounds=10, nthread=1, verbose=0, max_depth=8,
                 subsample=0.8358, min_child_weight=0.9225,
                 colsample_bytree=0.9852, eta=0.2885)
  # 保存模型,设置class为目标列
  if(!is.null(instance)){
    learner$param_set$values = instance$result_learner_param_vals
  }
  task_train <- train %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"))
  task_predict <- test %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"))
  # 按比例划分为训练集与测试集
  train_set <- partition(task_train, ratio = 1)$train
  test_set <- partition(task_predict, ratio = 0)$test
  # 设置多线程运算
  set_threads(learner)
  # 模型训练
  learner$train(task_train, row_ids = train_set)
  # 测试集预测
  predict <- learner$predict(task_predict, row_ids = test_set)
  # 结果评估
  print(predict$score(measure))
  # 储存模型
  list(learner, predict)
}
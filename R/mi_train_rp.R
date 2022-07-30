#' Classification tree model training.
#'
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param train A dataframe.
#' @param test A dataframe.
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @export
mi_train_rp <- function(train, test, measure = msr("classif.acc")) {
  learner <- lrn("classif.rpart", keep_model = TRUE,xval=0, cp=0.0005253, maxdepth=24,
                 minsplit=4, maxcompete=3)
  # 保存模型,设置class为目标列
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
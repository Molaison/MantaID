#' Compare classification models with small samples.
#'
#' @param data A tibble.All are numeric except the first column is a factor.
#'
#' @param row_num Number of samples used.
#' @param resamplings R6/Resampling.Resampling method.
#' @importFrom dplyr slice select across mutate
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrns benchmark_grid set_threads benchmark msr rsmps
#' @export
#' @examples
#' data(ProcData)
mt_run_bmr <- function(data, row_num = 4000, resamplings = rsmps("cv", folds = 10)) {
  data <- data %>% mutate(across(.cols = -class,.fns = as.numeric))
  if(nrow(data)<row_num){
    row_num = nrow(data)
  }
  task <- data %>%
    # 抽取一定量样本作为训练集
    slice(sample(nrow(data), row_num), preserve = TRUE) %>%
    # tibble 转为 data.table
    as.data.table() %>%
    # 目标类为class,其余作为特征列
    as_task_classif(target = "class", feature = -c("class"))
  # 随机森林、朴素贝叶斯学习器
  learners <- lrns(c("classif.naive_bayes", "classif.rpart", "classif.ranger", "classif.xgboost", "classif.kknn", "classif.multinom"),
    predict_type = "prob",
    predict_sets = c("train", "test") # 训练集与测试集均用作预测,分别评估;
  )
  bmr_g <- benchmark_grid(
    tasks = task,
    learners = learners,
    resamplings = resamplings
    # bootstrap, custom, custom_cv, cv, holdout, insample, loo,repeated_cv, subsampling
  )
  set_threads(bmr_g)
  # 基准测试
  bmr <- benchmark(bmr_g)
  # 评估结果,分别计算训练集的cost和测试集的ce
  measures <- msr("classif.acc")
  scores <- bmr$score(measures) %>%
    as.data.table() %>%
    select(ncol(.), "learner_id", everything())
  # 储存基准测试结果
  list(bmr, scores)
}
#' Classification tree model training.
#'
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param train A dataframe.
#' @param test A dataframe.
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @export
mt_train_rp <- function(train, test, measure = msr("classif.acc")) {
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
#' Random Forest Model Training.
#'
#' @param measure Model evaluation method.
#' @param train A dataframe.
#' @param test A dataframe.
#'
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @export
mt_train_rg <- function(train, test, measure = msr("classif.acc")) {
  learner <- lrn("classif.ranger",importance='impurity',
                 regularization.factor=0.01407, minprop=0.01667, num.trees=385,
                 max.depth=368)
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

#' Xgboost model training
#'
#' @param measure Model evaluation method.
#' @param train A dataframe.
#' @param test A dataframe.
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @export
mt_train_xgb <- function(train, test, measure = msr("classif.acc")) {
  learner <- lrn("classif.xgboost",nrounds=10, nthread=1, verbose=0, max_depth=8,
                 subsample=0.8358, min_child_weight=0.9225,
                 colsample_bytree=0.9852, eta=0.2885)
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


#' Train a three layers neural network model.
#'
#' @param train A dataframe with `class` column as label.
#' @param test A dataframe with `class` column as label.
#' @param path2save The folder path to store the model and train history.
#' @param batch_size Integer or NULL.Number of samples per gradient update.
#' @param epochs Number of epochs to train the model.
#' @param cls A character.The name of the label column.
#' @param validation_split Float between 0 and 1. Fraction of the training data to be used as validation data.
#'
#' @importFrom dplyr rename mutate across select
#' @importFrom keras to_categorical layer_activation_relu layer_dense keras_model_sequential save_model_tf k_argmax evaluate optimizer_adam fit compile
#' @importFrom caret confusionMatrix
#' @importFrom magrittr %>%
#' @importFrom stats predict
#' @importFrom stringr str_c
#' @return A `list` object th at contains the prediction confusion matrix and the `model` object.
#' @export

BPNN <- function(train, test, cls = "class", path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3) {
  train <- train %>%
    rename("class" = cls)
  test <- test %>%
    rename("class" = cls)
  train_set <- train %>%
    mutate(across(.cols = -class, .fns = as.numeric)) %>%
    select(-class) %>%
    as.matrix()
  test_set <- test %>%
    mutate(across(.cols = -class, .fns = as.numeric)) %>%
    select(-class) %>%
    as.matrix()

  train_target <- train$class %>%
    as.numeric() %>%
    to_categorical() %>%
    magrittr::set_names(levels(test$class))
  test_target <- test$class %>%
    as.numeric() %>%
    to_categorical() %>%
    magrittr::set_names(levels(test$class))
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 40, input_shape = ncol(train_set)) %>%
    layer_activation_relu() %>%
    layer_dense(units = 40) %>%
    layer_activation_relu() %>%
    layer_dense(units = ncol(train_target), activation = "softmax")
  summary(model)

  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = list("categorical_accuracy")
  )
  history <- model %>% fit(
    x = train_set, y = train_target,
    epochs = epochs, batch_size = batch_size,
    validation_split = 0.3, verbose = 2
  )
  # save_model_tf(model, "result_net_usmld")
  if(!is.null(path2save)){
    save_model_tf(model, str_c(path2save, "/result_net"))
  }

  # model <- load_model_tf("DeepLearning_Model/result_net/")
  predictions <- predict(model, test_set)
  response <- predictions %>% k_argmax()
  response <- response$numpy() %>%
    as.numeric() %>%
    levels(test$class)[.] %>%
    factor(levels = levels(test$class))
  prd_net <- confusionMatrix(response, test$class)
  score <- model %>% evaluate(test_set, test_target)
  return(list(model, prd_net))
}

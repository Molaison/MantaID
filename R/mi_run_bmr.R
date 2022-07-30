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
#' data(mi_data_procID)
mi_run_bmr <- function(data, row_num = 4000, resamplings = rsmps("cv", folds = 10)) {
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
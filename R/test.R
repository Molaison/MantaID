mi <- function(cores = NULL,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"),ratio = 0.3,para_blc = F, model_path = NULL, batch_size = 128, epochs = 64, validation_split = 0.3,graph_path=NULL){
    pad_len = mi_get_padlen(mi_data_allID)
    data_split = mi_split_col(mi_data_allID,cores = cores,pad_len = pad_len)
    data_num = mi_to_numer(data_splt,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"))
    data_blcd = mi_balance_data(data_num,ratio = 0.3,parallel = F)
    train = data_blcd[[1]]
    test = data_blcd[[2]]
    result_rg <- mi_train_rg(train, test, measure = msr("classif.acc"))
    result_rp <- mi_train_rp(train, test, measure = msr("classif.acc"))
    result_xgboost <- mi_train_xgb(train, test, measure = msr("classif.acc"))
    result_net <- mi_train_BP(train, test, path2save = path2save, batch_size = batch_size, epochs = epochs, validation_split = validation_split)
    matri_rg <- mi_get_confusion(result_rg)
    matri_rp <- mi_get_confusion(result_rp)
    matri_xgb <- mi_get_confusion(result_xgb)
    matri_net <- mi_get_confusion(result_net,ifnet = T)
    mi_plot_heatmap(matri_rg, name="rg",filepath = graph_path)
    mi_plot_heatmap(matri_rp, name="rp",filepath = graph_path)
    mi_plot_heatmap(matri_xgb, name="xgb",filepath = graph_path)
    mi_plot_heatmap(matri_net, name="xgb",filepath = graph_path)
}#' Data balance.
#' Most classes adopt random undersampling, while a few classes adopt smote method to oversample to obtain relatively balanced data;
#'
#' @param data A data frame. Except class column, all are numeric types.
#' @param ratio Numeric between 0 and 1. The percent of test set split from data.
#' @param parallel Logical.
#'
#' @return A list contain train set and test set.
#' @export
#' @importFrom scutr SCUT_parallel SCUT oversample_smote resample_random
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif partition
#' @importFrom dplyr slice bind_rows
#' @importFrom magrittr set_names
#' @examples
#' library(dplyr)
#' data = rename(iris,class =Species)
#' mi_balance_data(data)
mi_balance_data <- function(data, ratio = 0.3, parallel = F) {
  system.time({
    # 多分类数据平衡,欠采样样本采用随机采样,过采样部分采用smote方法采样,获得相对平衡的数据;
    if(parallel){
      data_smtd <- SCUT_parallel(data, "class", oversample = oversample_smote, undersample = resample_random)
    }else{
      data_smtd <- SCUT(data, "class", oversample = oversample_smote, undersample = resample_random)
    }
  })
  #差分平衡后数据集与平衡前数据集,作为训练集
  train_new <- setdiff(data_smtd, data)
  print(train_new %>% nrow())
  task <- data %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -"class")
  # 原数据集中分离出一部分同样作为训练集
  train_raw <-partition(task, ratio = 1-ratio)$train %>% slice(data, .)
  #训练集数据是原来数据集的拆分加上采样获得的新样本
  train <- bind_rows(train_raw, train_new)
  # 测试集数据来自原来数据集的拆分
  test <-partition(task, ratio = 1-ratio)$test %>% slice(data, .)

  return(list(train, test))
}#' Reshape data and delete meaningless rows.
#'
#' @param data A dataframe or tibble or data.table or matrix. Names of the column will be regard as the class of ID included in column.
#' @param placeholder Character vectors. IDs included in `placeholder` will be omitted.
#' @param cols Character vectors. Columns of `data` that contain the IDs
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select mutate_at pull slice
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom magrittr %<>%
#' @export
#' @return A tibble with two columns("ID" and "class")
#' @examples
#' data <- tibble::tibble(
#' "class1" = c("A","B","C","D"),
#' 'class2' = c("E","F","G","H"),
#' 'class3' = c("L","M","-","O")
#' )
#' mi_clean_data(data)
mi_clean_data <- function(data, cols = everything(), placeholder = c("-")) {
  data %<>% as_tibble() %>%
    select(cols) %>%
    mutate_at(colnames(.), ~ as.character(.x)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "class",
      values_to = "ID",
      names_repair = "minimal"
    ) %>%
    select("ID", "class")
  index <- map(data %>% pull("ID"), ~ !.x %in% placeholder)
  data %>% slice(which(index == TRUE))
}#' ID-related datasets in biomart.
#'
#' @format A dataframe with 65 variables and 3 variables.
#' \describe{
#' 	\item{name}{The name of dataset.}
#' 	\item{description}{Description of dataset.}
#' 	\item{page}{collection of attributes.}
#' }
"mi_data_attributes"
#' @title Processed ID data.
#' @format A tibble dataframe with 5000 rows and 21 variables.
#' \describe{
#'   \item{pos1 to pos20}{Splited ID.}
#'   \item{class}{The databases that ID belongs to.}
#' }
"mi_data_procID"
#' @title ID dataset for testing.
#' @format A tibble with 5000 rows and 2 variables.
#' \describe{
#' \item{ID}{A identifier character.}
#' \item{class}{The database the ID belongs to.}
#' }
"mi_data_rawID"
#' Get ID data from `Biomart` database use `attributes`.
#'
#' @param dataset Datasets of the selected BioMart database.
#' @param mirror Specify an Ensembl mirror to connect to.
#' @param biomart BioMart database name you want to connect to.Use `biomaRt::listEnsembl` to retrieve the possible database names.
#' @param attributes A dataframe.The information we want to retrieve.Use `mi_get_ID_attr` to hava try.
#'
#' @importFrom biomaRt useEnsembl getBM
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across rename select
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @return A `tibble` dataframe.
#' @export
#'
#' @examples
#' data(mi_data_attributes)
#' mi_get_ID(mi_data_attributes[1:3, ])
mi_get_ID <- function(attributes, biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
    Ensembl <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  out <- vector("list", length = nrow(attributes))
  for (i in 1:nrow(attributes)) {
    try_result <- try({
      out[[i]] <- getBM(attributes = unique(attributes[["name"]])[i], mart = Ensembl)
      sprintf("The %dth getFunc successed!", i)
    })
    if ("try-error" %in% class(try_result)) {
      next
    }
    Sys.sleep(0.5)
  }
  to_2col <- function(df) {
    df %>%
      as_tibble() %>%
      mutate(class = colnames(.)[1]) %>%
      rename(ID = 1) %>%
      mutate(across(.cols = 1, .fns = as.character))
  }
  map_dfr(out, .f = to_2col)
}#' Get ID attributes from `Biomart` database.
#'
#' @param dataset Datasets of the selected BioMart database.
#' @param mirror Specify an Ensembl mirror to connect to.
#' @param biomart BioMart database name you want to connect to.Use `biomaRt::listEnsembl` to retrieve the possible database names.
#' @importFrom dplyr filter
#' @importFrom stringr str_length
#' @importFrom biomaRt useEnsembl listAttributes
#' @importFrom magrittr %>%
#' @return A dataframe.
#' @export
mi_get_ID_attr <- function(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
  # 使用asia mirror获取hsapiens_gene_ensembl数据库
  ensemb_hm_dset <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  attributes <- listAttributes(ensemb_hm_dset) %>%
    # 筛选可能包含ID的属性
    filter(grepl(.[["description"]], pattern = "(id)|(name)", ignore.case = TRUE)) %>%
    # 去除一些不需要的属性
    filter(!grepl(.[["description"]], pattern = "(end)|(start)|(description)|(probe)|(version)|(content)|(Aberrant)|(Source)|(Strain)|(Chromosome)", ignore.case = TRUE)) %>%
    # 名字过长的属性往往不是id,而是基因的位置等信息对应的索引等
    filter(str_length(.[["name"]]) < 18)
}#' Compute the confusion matrix for the predict result.
#'
#' @param ifnet Logical.Whether the data is obtained by a deep learning model.
#' @param result_list A list return from model training functions.
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr select
#' @importFrom caret confusionMatrix
#' @return A `confusionMatrix` object.
#' @export
#'
mi_get_confusion <- function(result_list, ifnet = F) {
  if (ifnet) {
    return(result_list[[2]])
  }
  matri_tr <- result_list[[2]] %>%
    as.data.table() %>%
    select(-1)
  confusionMatrix(matri_tr %>% pull(2), matri_tr %>% pull(1))
}#' Observe the distribution of the false response of test set.
#' @param predict A R6 class `PredictionClassif`.
#' @importFrom data.table as.data.table
#' @importFrom dplyr filter group_by count
#' @importFrom magrittr %>% %T>%
#' @export
#' @return A tibble data frame that records the number of wrong predictions for each category ID;
mi_get_miss <- function(predict) {
  stopifnot(any(class(predict) == "PredictionClassif"))
  result <- predict %>%
    as.data.table() %>%
    filter("truth" != "response") %>%
    group_by("truth") %>%
    count() %T>%
    print()
}#' Get max length of ID data.
#'
#' @param data A dataframe.
#' @importFrom dplyr pull
#' @return A int.
#' @export
mi_get_padlen <- function(data){
  data %>%
  pull("ID") %>%
  map(nchar) %>%
  unlist() %>%
  max()
}#' Plot heatmap for result confusion matrix.
#'
#' @param table A table.
#' @param filepath File path the plot to save.Default NULL.
#' @param name Model names.
#'
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate group_by ungroup across na_if
#' @importFrom ggplot2 coord_equal ggplot aes geom_text geom_tile theme_bw coord_equal scale_fill_gradient2 labs theme element_text ggsave
#' @importFrom stringr str_c
#' @importFrom RColorBrewer brewer.pal
#' @return A `ggplot` object.
#' @export
mi_plot_heatmap <- function(table,name = NULL, filepath = NULL) {
  melted <- table %>%
    as.table() %>%
    melt() #%>%
    # group_by(.data[['Reference']]) %>%
    # mutate(across(.cols = .data[['value']], .fns = ~ .x / sum(.x))) %>%
    # ungroup()

  heat <- ggplot(melted, aes(x = .data[['Reference']], y =.data[['Prediction']], fill = .data[['value']])) +
    geom_tile() +
    geom_text(aes(label = na_if(round(.data[['value']], 2), 0)), color = "#00468B99", size = 2) +
    theme_bw() +
    coord_equal() +
    # scale_fill_gradient2(low="#003366", high="#990033",mid = "white") +
    #scale_fill_gradient2(name = NULL, high = "#ED000099", mid = "white") +
    scale_fill_gradientn(colors = c("#ffffff",brewer.pal(5, "YlOrRd")),values = c(0,4e-04,4e-03,4e-02,0.2,1))+
    theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = 1, size = 20, hjust = 1,
      lineheight = 10
    ),
    axis.text.y = element_text(size = 20),
    strip.text.y = element_text(
      angle = 0,
      vjust = 0.5,
      hjust = 0.5,
      size = 10
    ),
    axis.title = element_text(size = 40)
  )
  if (!is.null(filepath)) {
    ggsave(str_c(filepath, "heatmap_", name, ".png"), width = 22.37, height = 20, plot = heat, device = "png", dpi = 600)
  }
  return(heat)
}#' Predict new data with trained learner.
#' @param data A dataframe.
#'
#' @param learner A R6 class object.
#' @importFrom dplyr rename mutate across select bind_cols
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @export
mi_predict_new <- function(data, learner) {
  data %>%
    mutate(across(.cols = everything(), .fns = as.numeric)) %>%
    # 将数据类型转为数值型
    learner$predict_newdata(newdata = ., task = NULL) # %>%
  # 调用学习器进行预测
  as.data.table() %>%
    # 将预测结果与数据组合
    bind_cols(data, .) %>%
    # 预测新数据,真实值未知
    select(-"truth") %>%
    # 将预测结果放到第一列
    select("response", everything()) %>%
    rename("response" = learner$id)
}#' Compare classification models with small samples.
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
}#' Cut the string of ID column character by character and divide it into multiple columns.
#'
#' @param data Dataframe(tibble) to be split.
#' @param cores Int.The num of cores to allocate for computing.
#' @param pad_len The length of longest id, i.e. the maxlength.
#' @importFrom parallel detectCores makeCluster clusterExport clusterEvalQ parSapply stopCluster
#' @importFrom  dplyr bind_cols
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#' @return A tibble with pad_len+1 column.
#' @export
#' @examples
#' data(mi_data_rawID)
#' mi_split_col(mi_data_rawID,cores = 1,pad_len = 10)
mi_split_col <- function(data, cores = NULL, pad_len = 10) {
  # 2.计算计算机内核数
  core_max <- detectCores(logical = FALSE)%/%2
  # 3.打开并行计算
  if (is.null(cores)) {
    cl <- makeCluster(core_max)
  } else {
    cl <- makeCluster(cores)
  }
  mi_split_str <- function(str, pad_len) {
    str %>%
      as.character() %>%
      strsplit(split = "") %>%
      unlist() %>%
      c(., rep("*", ifelse((pad_len - length(.)) > 0, pad_len - length(.), 0))) %>%
      .[1:pad_len]
  }
  # 4.给每个单独内核传递变量,函数等
  clusterExport(cl, varlist = c("pad_len", "mi_split_str"), envir = environment())
  clusterEvalQ(cl, c(library(data.table), library(magrittr), library(stringr),library(dplyr)))
  # 5.开始并行计算（用法与sapply类似）
  output <- parSapply(cl, data[, 1][[1]], mi_split_str, pad_len)
  # 6.关闭并行计算
  stopCluster(cl)
  output %>%
    unlist() %>%
    matrix(byrow = TRUE, ncol = pad_len) %>%
    bind_cols(data[, 2]) %>%
    set_names(c(str_c("pos", 1:pad_len),"class"))
}#' Split the string into individual characters and complete the character vector to the maximum length.
#'
#' @param str The string to be splited.
#' @param pad_len The length of longest id, i.e. the maxlength.
#' @export
#' @return Splited character vector.
#' @examples
#' string_test = "Good Job"
#' length = 15
#' mi_split_str(string_test,length)
mi_split_str <- function(str, pad_len) {
  str %>%
    as.character() %>%
    strsplit(split = "") %>%
    unlist() %>%
    c(., rep("*", pad_len - length(.)))
}#' Convert data to numeric, and for ID column convert with fixed levels.
#' @export
#' @importFrom  dplyr across mutate
#' @param data A tibble with n position column(pos1,pos2,...) and class column.
#' @param levels Characters accommodated in IDs.
#' @examples
#' data(mi_data_rawID)
#' str(mi_split_col(mi_data_rawID,cores = 1,pad_len = 10))
#' str(mi_to_numer(mi_split_col(mi_data_rawID,cores = 1,pad_len = 10)))
mi_to_numer <- function(data, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":")) {
  data %>%
    mutate(across(.cols = -"class", .fns = ~ factor(.x, levels = levels))) %>%
    mutate(across(.cols = "class", .fns = factor)) %>%
    mutate(across(.cols = -"class", .fns = as.numeric))
}#' Train a three layers neural network model.
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
mi_train_BP <- function(train, test, cls = "class", path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3) {
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
}#' Random Forest Model Training.
#'
#' @param measure Model evaluation method.
#' @param train A dataframe.
#' @param test A dataframe.
#'
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @export
mi_train_rg <- function(train, test, measure = msr("classif.acc"),param = NULL) {
  learner <- lrn("classif.ranger",importance='impurity',
                 regularization.factor=0.01407, minprop=0.01667, num.trees=385,
                 max.depth=368)
  if(!is.null(param)){
    learner$
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
}#' Classification tree model training.
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
}#' Xgboost model training
#'
#' @param measure Model evaluation method.
#' @param train A dataframe.
#' @param test A dataframe.
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn  set_threads msr partition
#' @export
mi_train_xgb <- function(train, test, measure = msr("classif.acc")) {
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
}#' Tune Random Forest model by hyperband.
#'
#' @param data A tibble.All are numeric except the first column is a factor.
#' @param resampling R6/Resampling.
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param eta The percent parameter configurations discarded.
#' @importFrom dplyr slice mutate pull group_by
#' @importFrom mlr3tuning tnr tune
#' @importFrom paradox ps p_dbl p_int
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn rsmp
#' @importFrom ggplot2 geom_line geom_point guides scale_x_continuous scale_y_continuous
#' @return A list of tuning instance and stage plot.
#' @export
mi_tune_rg <- function(data, resampling = rsmp("cv", folds = 5), measure = msr("classif.acc"), eta = 3) {
	search_space <- ps(
		regularization.factor = p_dbl(lower = 0.01, upper = 1),
		minprop = p_dbl(lower = 0.005, upper = 0.15, tags = "budget"),
		num.trees = p_int(lower = 100, upper = 600),
		max.depth = p_int(lower = 20, upper = 400)
	)
	data %<>% slice(sample(nrow(.), nrow(.)))
	learner <- lrn("classif.ranger", importance = "impurity", save.memory = F, oob.error = F, num.threads = 12)
	task <- data %>%
		as.data.table() %>%
		as_task_classif(target = "class", feature = -c("class"))
	instance <- tune(
		method = tnr("hyperband", eta = 3),
		task = task,
		learner = learner,
		resampling = resampling,
		measures = measure,
		search_space = search_space
	)
	result = instance$archive$data
	hyperband_group <- result %<>% bind_cols(hyperband = str_c(result$regularization.factor,result$num.trees,result$max.depth)) %>% mutate("stage" = .data[["stage"]] + 1)
	fct <- hyperband_group %>%
		pull(.data[['hyperband']]) %>%
		factor()
	result <- hyperband_group %>% split(fct)
	p <- ggplot(data = hyperband_group, mapping = aes(x = .data[["stage"]], y = .data[["classif.acc"]], group =  .data[["hyperband"]], colour = factor(.data[['hyperband']]))) +
		scale_x_continuous(limits = c(0.5, max(hyperband_group$stage) + 0.5)) +
		scale_y_continuous(limits = c(min(hyperband_group$classif.acc), max(hyperband_group$classif.acc))) +
		theme_bw() +
		guides(color = FALSE) +
		geom_point() +
		geom_line()
	return(list(instance, p))
}#' Tune decision tree model by hyperband.
#'
#' @param data A tibble.All are numeric except the first column is a factor.
#' @param resampling R6/Resampling.
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param eta The percent parameter configurations discarded.
#' @importFrom dplyr slice mutate pull group_by
#' @importFrom mlr3tuning tnr tune
#' @importFrom paradox ps p_dbl p_int
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn rsmp
#' @importFrom ggplot2 geom_line geom_point guides scale_x_continuous scale_y_continuous
#' @return A list of tuning instance and stage plot.
#' @export
mi_tune_rp <- function(data, resampling = rsmp("bootstrap", ratio = 0.8, repeats = 5), measure = msr("classif.acc"), eta = 3) {
	search_space <- ps(
		cp = p_dbl(lower = 0.0005, upper = 0.01),
		maxdepth = p_int(lower = 10, upper = 30),
		minsplit = p_int(lower = 1, upper = 40, tags = "budget"),
		maxcompete = p_int(lower = 1, upper = 10)
	)
	data %<>% slice(sample(nrow(.), nrow(.)))
	learner <- lrn("classif.rpart", keep_model = F)
	task <- data %>%
		as.data.table() %>%
		as_task_classif(target = "class", feature = -c("class"))
	instance <- tune(
		method = tnr("hyperband", eta = 3),
		task = task,
		learner = learner,
		resampling = resampling,
		measures = measure,
		search_space = search_space
	)
	result = instance$archive$data
	hyperband_group <- result %<>% bind_cols(hyperband = str_c(result$maxdepth,result$maxcompete,result$cp)) %>% mutate("stage" = .data[["stage"]] + 1)
	fct <- hyperband_group %>%
		pull(.data[['hyperband']]) %>%
		factor()
	result <- hyperband_group %>% split(fct)
	p <- ggplot(data = hyperband_group, mapping = aes(x = .data[["stage"]], y = .data[["classif.acc"]], group =  .data[["hyperband"]], colour = factor(.data[['hyperband']]))) +
		scale_x_continuous(limits = c(0.5, max(hyperband_group$stage) + 0.5)) +
		scale_y_continuous(limits = c(min(hyperband_group$classif.acc), max(hyperband_group$classif.acc))) +
		theme_bw() +
		guides(color = FALSE) +
		geom_point() +
		geom_line()
	return(list(instance, p))
}globalVariables(".data")
globalVariables(".")
#' Tune Xgboost model by hyperband.
#'
#' @param data A tibble.All are numeric except the first column is a factor.
#' @param resampling R6/Resampling.
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param eta The percent parameter configurations discarded.
#' @importFrom dplyr slice mutate pull group_by
#' @importFrom mlr3tuning tnr tune
#' @importFrom paradox ps p_dbl p_int
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn rsmp
#' @importFrom ggplot2 geom_line geom_point guides scale_x_continuous scale_y_continuous
#' @return A list of tuning instance and stage plot.
#' @export
mi_tune_xgb <- function(data, resampling = rsmp("cv", folds = 5), measure = msr("classif.acc"), eta = 3) {
	search_space <- ps(
		max_depth = p_int(lower = 6, upper = 12),
		subsample = p_dbl(lower = 0.8, upper = 1),
		min_child_weight = p_dbl(lower = 0.8, upper = 1.2),
		colsample_bytree = p_dbl(lower = 0.8, upper = 1),
		eta = p_dbl(lower = 0, upper = 0.3),
		nrounds = p_int(lower = 1, upper = 40, tags = "budget")
	)
	data %<>% slice(sample(nrow(.), nrow(.)))
	learner <- lrn("classif.xgboost", nthread = 12)
	task <- data %>%
		as.data.table() %>%
		as_task_classif(target = "class", feature = -c("class"))
	instance <- tune(
		method = tnr("hyperband", eta = 3),
		task = task,
		learner = learner,
		resampling = resampling,
		measures = measure,
		search_space = search_space
	)
	result = instance$archive$data
	hyperband_group <- result %<>% bind_cols(hyperband = str_c(result$max_depth,result$subsample,result$min_child_weight,result$eta)) %>% mutate("stage" = .data[["stage"]] + 1)
	fct <- hyperband_group %>%
		pull('hyperband') %>%
		factor()
	result <- hyperband_group %>% split(fct)
	p <- ggplot(data = hyperband_group, mapping = aes(x = .data[["stage"]], y = .data[["classif.acc"]], group =  .data[["hyperband"]], colour = factor(.data[['hyperband']]))) +
		scale_x_continuous(limits = c(0.5, max(hyperband_group$stage) + 0.5)) +
		scale_y_continuous(limits = c(min(hyperband_group$classif.acc), max(hyperband_group$classif.acc))) +
		theme_bw() +
		guides(color = FALSE) +
		geom_point() +
		geom_line()
	return(list(instance, p))
}
#' A wrapper function that executes MantaID workflow.
#'
#' @param cores The number of cores used when balancing data.
#' @param levels The vector that includes all the single characters occurred in IDs.
#' @param ratio The ratio of the test set.
#' @param para_blc A logical value whether using parallel computing when balancing data.
#' @param model_path The path to save models.
#' @param batch_size The batch size of deep learning model fitting.
#' @param epochs The epochs of deep learning model fitting.
#' @param validation_split The validation ratio of deep learning model fitting.
#' @param graph_path The path to save graphs.
#' @param mi_data_allID IDs raw data.
#'
#' @return The list of models and graphs.
#' @export
mi <- function(mi_data_allID, cores = NULL, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"), ratio = 0.3, para_blc = FALSE, model_path = NULL, batch_size = 128, epochs = 64, validation_split = 0.3, graph_path = NULL) {
  #Get max length of ID data.
  pad_len <- mi_get_padlen(mi_data_allID)
  #Cut the string of ID column character by character and divide it into multiple columns.
  data_split <- mi_split_col(mi_data_allID, cores = cores, pad_len = pad_len)
  #Convert data to numeric, and for the ID column convert with fixed levels.
  data_num <- mi_to_numer(data_split, levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"))
  #Plot correlation heatmap.
  cor_plot <- mi_plot_cor(data_num, "class")
  #Data balance.
  data_blcd <- mi_balance_data(data_num, ratio = 0.3, parallel = FALSE)
  #Select the test set.
  train <- data_blcd[[1]]
  #Select the test set.
  test <- data_blcd[[2]]
  ##Tune the decision tree model by hyperband.
  inst_rp <- mi_tune_rp(train, test)
  #Tune the random forest model by hyperband.
  inst_rg <- mi_tune_rg(train, test)
  #Tune the xgboost model by hyperband.
  inst_xgb <- mi_tune_xgb(train, test)
  #Output the stage plot of the decision tree model.
  stageplot_rp <- inst_rp[[2]]
  #Output the stage plot of the random forest model.
  stageplot_rg <- inst_rg[[2]]
  ##Output the stage plot of the xgboost model.
  stageplot_xgb <- inst_xgb[[2]]
  #Train the decision tree model.
  result_rp <- mi_train_rp(train, test, measure = msr("classif.acc"), instance = inst_rp[[1]])
  #Train the random forest model.
  result_rg <- mi_train_rg(train, test, measure = msr("classif.acc"), instance = inst_rg[[1]])
  #Train the xgboost model.
  result_xgb <- mi_train_xgb(train, test, measure = msr("classif.acc"), instance = inst_xgb[[1]])
  #Train the neural network model.
  result_net <- mi_train_BP(train, test, path2save = graph_path, batch_size = batch_size, epochs = epochs, validation_split = validation_split)
  #Obfuscation matrix about the decision tree is obtained.
  matri_rp <- mi_get_confusion(result_rp)
  #Obfuscation matrix about random forest is obtained.
  matri_rg <- mi_get_confusion(result_rg)
  #Obfuscation matrix about xgboost is obtained.
  matri_xgb <- mi_get_confusion(result_xgb)
  #Obfuscation matrix about the neural network is obtained.
  matri_net <- mi_get_confusion(result_net, ifnet = TRUE)
  #Confusion matrix heatmap on the decision tree.
  heatmap_rp <- mi_plot_heatmap(matri_rp, name = "rp", filepath = graph_path)
  #Confusion matrix heatmap on the random forests.
  heatmap_rg <- mi_plot_heatmap(matri_rg, name = "rg", filepath = graph_path)
  #Confusion matrix heatmap on the xgboost.
  heatmap_xgb <- mi_plot_heatmap(matri_xgb, name = "xgb", filepath = graph_path)
  #Confusion matrix heatmap on the neural network.
  heatmap_net <- mi_plot_heatmap(matri_net, name = "net", filepath = graph_path)
  #Output the learners for predicting as a list.
  learners <- list(rp = result_rp[[1]], rg = result_rg[[1]], xgb = result_xgb[[1]], BP = result_net[[1]])
  #Output the heatmaps as a list.
  heatmaps <- list(rp = heatmap_rp, rg = heatmap_rg, xgb = heatmap_xgb, BP = heatmap_net)
  #Output the stage plots as a list.
  stageplots <- list(rp = stageplot_rp, rg = stageplot_rg, xgb = stageplot_xgb)
  #Output the learners, heatmaps and stage plots as a list.
  return(list(learners, heatmaps, stageplots))
}

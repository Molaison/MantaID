#' Compute the confusion matrix for the predicted result.
#'
#' @param ifnet Logical.Whether the data is obtained by a deep learning model.
#' @param result_list A list returned from model training functions.
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr select
#' @importFrom caret confusionMatrix
#' @return A `confusionMatrix` object.
#' @export
mi_get_confusion <- function(result_list, ifnet = FALSE) {
  #If data obtained by a deep learning model exists, the prediction is returned.
  if (ifnet) {
    return(result_list[[2]])
  }
  #Convert the prediction results into a table by removing the first column and converting it into matrix form.
  matri_tr <- result_list[[2]] %>%
    as.data.table() %>%
    select(-1)
  #Extract the second column of the matrix and the first column of the matrix as the rows and columns of the confusion matrix, respectively.
  confusionMatrix(matri_tr %>% pull(2), matri_tr %>% pull(1))
}

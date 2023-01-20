#' Train a three layers neural network model.
#'
#' @param train A dataframe with the `class` column as label.
#' @param test A dataframe with the `class` column as label.
#' @param path2save The folder path to store the model and train history.
#' @param batch_size Integer or NULL. The number of samples per gradient update.
#' @param epochs The number of epochs to train the model.
#' @param cls A character.The name of the label column.
#' @param validation_split Float between 0 and 1. Fraction of the training data to be used as validation data.
#'
#' @importFrom dplyr rename mutate across select
#' @importFrom keras to_categorical layer_activation_relu layer_dense keras_model_sequential save_model_tf k_argmax evaluate optimizer_adam fit compile
#' @importFrom caret confusionMatrix
#' @importFrom magrittr %>%
#' @importFrom stats predict
#' @importFrom stringr str_c
#' @return A `list` object containing the prediction confusion matrix, the `model` object, and the mapping of predicted numbers to classes.
#' @export
mi_train_BP <- function(train, test, cls = "class", path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3) {
  #Rename the column names of the cls columns of the training set.
  train <- train %>%
    dplyr::rename("class" = cls)
  #Rename the column names of the cls columns of the test set.
  test <- test %>%
   dplyr::rename("class" = cls)
  #The class column of the training set is removed and the values of the other columns are converted to numerical form, and then converted to matrix form to redefine it as the training set.
  train_set <- train %>%
    mutate(across(.cols = -class, .fns = as.numeric)) %>%
    select(-class) %>%
    as.matrix()
  #Remove the class column of the test set and convert the values of the other columns to numeric form, then convert them to matrix form and redefine them as a test set.
  test_set <- test %>%
    mutate(across(.cols = -class, .fns = as.numeric)) %>%
    select(-class) %>%
    as.matrix()
  #Convert the class columns of the training set into factor types, then into numeric types, and finally into binary class matrices.
  train_target <- train$class %>%
    factor() %>%
    as.numeric() %>%
    to_categorical()
  train_target <- train_target[, -c(1)]
  test_target <- test$class %>%
    factor() %>%
    as.numeric() %>%
    to_categorical()
  test_target <- test_target[, -c(1)]
  #Define a keras sequential model.
  model <- keras_model_sequential()
  #The input layer, feature layer, task layer, and activation layer are assembled into a Model.
  model %>%
    layer_dense(units = 40, input_shape = ncol(train_set)) %>%
    layer_activation_relu() %>%
    layer_dense(units = 40) %>%
    layer_activation_relu() %>%
    layer_dense(units = ncol(train_target), activation = "softmax")
  summary(model)
  #Compile the defined model with metric = accuracy and optimiser as adam.
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = list("categorical_accuracy")
  )
  #Fit the model on the training dataset
  history <- model %>% fit(
    x = train_set, y = train_target,
    epochs = epochs, batch_size = batch_size,
    validation_split = 0.3, verbose = 2
  )
  #Save model
  if (!is.null(path2save)) {
    save_model_tf(model, str_c(path2save, "/result_net"))
  }
  #Predict model.
  predictions <- predict(model, test_set)
  #Return the index of the maximum value.
  response <- predictions %>% k_argmax()
  #Create multi-dimensional arrays and convert them to numeric types.
  response <- response$numpy() %>%
    as.numeric(.)
  #To get the levels
  level <- levels(test$class)
  #Modify level and convert to factor type.
  response <- level[response + 1] %>% factor(level)
  #Create a confusion matrix
  prd_net <- confusionMatrix(response, test$class)
  #Evaluate model
  score <- model %>% evaluate(test_set, test_target)
  #Output the model, confusion matrix, and level as a list.
  return(list(model, prd_net, level))
}

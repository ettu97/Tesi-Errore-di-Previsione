
#### FUNZIONI AUSILIARI ####

#### LDA ####

# FUNZIONE TRAIN
train_lda <- function(x, y) {
  model <- lda(x, grouping = y)
  return(model)
}

# FUNZIONE CLASSIFY
classify_lda <- function(object, newdata) {
  predictions <- predict(object, newdata)$class
  return(predictions)
}

#### QDA ####

# FUNZIONE TRAIN
train_qda <- function(x, y) {
  model <- qda(x, grouping = y)
  return(model)
}

# FUNZIONE CLASSIFY
classify_qda <- function(object, newdata) {
  predictions <- predict(object, newdata)$class
  return(predictions)
}

#### KNN ####

# FUNZIONE TRAIN
train_3nn<- function(x, y, k = 3) {
  list(train_data = x, train_labels = y, k = k)
}

train_5nn<- function(x, y, k = 5) {
  list(train_data = x, train_labels = y, k = k)
}

train_7nn <- function(x, y, k = 7) {
  list(train_data = x, train_labels = y, k = k)
}

train_9nn <- function(x, y, k = 9) {
  list(train_data = x, train_labels = y, k = k)
}

# FUNZIONE CLASSIFY
classify_knn <- function(object, newdata) {
  knn(train = object$train_data, test = newdata, cl = object$train_labels, k = object$k)
}


#### ALBERI DECISIONALI ####

library(rpart)

# FUNZIONE TRAIN
train_tree <- function(x, y) {
  data <- data.frame(x, y)
  colnames(data)[ncol(data)] <- "y"
  tree_model <- rpart(y ~ ., data = data)
  return(tree_model)
}

# FUNZIONE CLASSIFY
classify_tree <- function(object, newdata) {
  newdata_df <- as.data.frame(newdata)
  predictions <- predict(object, newdata_df, type = "class")
  return(predictions)
}

#####################################

train_tree2 <- function(x, y) {
  data <- data.frame(x, y)
  colnames(data)[ncol(data)] <- "y"
  tree_model <- tree(y ~ ., data = data)  # Using tree instead of rpart
  return(tree_model)
}


classify_tree2 <- function(object, newdata) {
  newdata_df <- as.data.frame(newdata)
  predictions <- predict(object, newdata_df, type = "class")  # Make sure the tree supports this type
  return(predictions)
}




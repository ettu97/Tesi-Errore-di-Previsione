library(caret)
library(class)
library(rpart)
library(tidyverse)
library(kableExtra)
library(knitr)
library(dplyr)
library(gridExtra)
library(MASS)
#### FUNZIONI AUSILIARI ####

#### LDA ####

lda_wrapper <- function(object, newdata) { predict(object, newdata)$class }

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

train_7nn <- function(x, y, k = 7) {
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

#### KNN K=3 ####
MSE_boot_3nn <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$MSE
MSE_632_3nn <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$MSE
MSE_632_plus_3nn <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$MSE
MSE_cv_3nn <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$MSE

BIAS_boot_3nn <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$Bias
BIAS_632_3nn <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$Bias
BIAS_632_plus_3nn <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$Bias
BIAS_cv_3nn <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_3nn, classify = classify_knn, seed = 123)$Bias

#### KNN K=7 ####
MSE_boot_7nn <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$MSE
MSE_632_7nn <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$MSE
MSE_632_plus_7nn <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$MSE
MSE_cv_7nn <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$MSE

BIAS_boot_7nn <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$Bias
BIAS_632_7nn <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$Bias
BIAS_632_plus_7nn <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$Bias
BIAS_cv_7nn <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_7nn, classify = classify_knn, seed = 123)$Bias


#### LDA ####
MSE_boot_lda <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$MSE
MSE_632_lda <- MSE_BIAS_632(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$MSE
MSE_632_plus_lda <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$MSE
MSE_cv_lda <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$MSE

BIAS_boot_lda <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$Bias
BIAS_632_lda <- MSE_BIAS_632(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$Bias
BIAS_632_plus_lda <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$Bias
BIAS_cv_lda <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = MASS:::lda , classify = lda_wrapper, seed = 123)$Bias


#### QDA ####
MSE_boot_qda <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$MSE
MSE_632_qda <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$MSE
MSE_632_plus_qda <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$MSE
MSE_cv_qda <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$MSE

BIAS_boot_qda <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$Bias
BIAS_632_qda <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$Bias
BIAS_632_plus_qda <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$Bias
BIAS_cv_qda <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_qda , classify = classify_qda, seed = 123)$Bias


#### TREE ####
MSE_boot_tree <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$MSE
MSE_632_tree <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$MSE
MSE_632_plus_tree <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$MSE
MSE_cv_tree <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$MSE

BIAS_boot_tree <- MSE_BIAS_boot(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$Bias
BIAS_632_tree <- MSE_BIAS_632(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$Bias
BIAS_632_plus_tree <- MSE_BIAS_632_plus(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$Bias
BIAS_cv_tree <- MSE_BIAS_CV(iris, classe = 5, R = 50, train = train_tree, classify = classify_tree, seed = 123)$Bias

######################################





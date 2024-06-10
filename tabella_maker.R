
#### TABELLA PER CONFRONTI ####

tabella_confronti <- function(dataset, classe, R = NULL, seed = NULL,...) {
  #### KNN K=3 ####
  boot_5nn <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_5nn, classify = classify_knn, seed = seed)
  b632_5nn <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_5nn, classify = classify_knn, seed = seed)
  b632_plus_5nn <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_5nn, classify = classify_knn, seed = seed)
  cv_5nn <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_5nn, classify = classify_knn, seed = seed)
  loocv_5nn <- MSE_BIAS_LOOCV(dataset, classe = classe, R = R, train = train_5nn, classify = classify_knn, seed = seed)
  bcv_5nn <- MSE_BIAS_BCV(dataset, classe = classe, R = R, train = train_5nn, classify = classify_knn, seed = seed)
  mccv50_5nn <- MSE_BIAS_MCCV_50(dataset, classe = classe, R = R, train = train_5nn, classify = classify_knn, seed = seed)
  
  #### KNN K=7 ####
  boot_7nn <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)
  b632_7nn <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)
  b632_plus_7nn <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)
  cv_7nn <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)
  loocv_7nn <- MSE_BIAS_LOOCV(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)
  bcv_7nn <- MSE_BIAS_BCV(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)
  mccv50_7nn <- MSE_BIAS_MCCV_50(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)
 
   #### LDA ####
  boot_lda <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_lda , classify = classify_lda, seed = seed)
  b632_lda <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_lda , classify = classify_lda, seed = seed)
  b632_plus_lda <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_lda , classify = classify_lda, seed = seed)
  cv_lda <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_lda , classify = classify_lda, seed = seed)
  loocv_lda <- MSE_BIAS_LOOCV(dataset, classe = classe, R = R, train = train_lda , classify = classify_lda, seed = seed)
  bcv_lda <- MSE_BIAS_BCV(dataset, classe = classe, R = R, train = train_lda , classify = classify_lda, seed = seed)
  mccv50_lda <- MSE_BIAS_MCCV_50(dataset, classe = classe, R = R, train = train_lda, classify = classify_lda, seed = seed)
  
  #### QDA ####
  boot_qda <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)
  b632_qda <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)
  b632_plus_qda <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)
  cv_qda <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)
  loocv_qda <- MSE_BIAS_LOOCV(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)
  bcv_qda <- MSE_BIAS_BCV(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)
  mccv50_qda <- MSE_BIAS_MCCV_50(dataset, classe = classe, R = R, train = train_qda, classify = classify_qda, seed = seed)
  
  #### TREE ####
  boot_tree <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)
  b632_tree <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)
  b632_plus_tree <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)
  cv_tree <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)
  loocv_tree <- MSE_BIAS_LOOCV(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)
  bcv_tree <- MSE_BIAS_BCV(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)
  mccv50_tree <- MSE_BIAS_MCCV_50(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)
  
  # dataframe
  data <- data.frame(
    MSE1_5nn = c(boot_5nn$MSE , b632_5nn$MSE, b632_plus_5nn$MSE, cv_5nn$MSE, loocv_5nn$MSE, bcv_5nn$MSE, mccv50_5nn$MSE), 
    Bias1_5nn = c(boot_5nn$Bias , b632_5nn$Bias, b632_plus_5nn$Bias, cv_5nn$Bias, loocv_5nn$Bias, bcv_5nn$Bias, mccv50_5nn$Bias),
    MSE2_7nn = c(boot_7nn$MSE , b632_7nn$MSE, b632_plus_7nn$MSE, cv_7nn$MSE, loocv_7nn$MSE, bcv_7nn$MSE, mccv50_7nn$MSE), 
    Bias2_7nn = c(boot_7nn$Bias , b632_7nn$Bias, b632_plus_7nn$Bias, cv_7nn$Bias, loocv_7nn$Bias, bcv_7nn$Bias, mccv50_7nn$Bias),
    MSE3_tree = c(boot_tree$MSE , b632_tree$MSE, b632_plus_tree$MSE, cv_tree$MSE, loocv_tree$MSE, bcv_tree$MSE, mccv50_tree$MSE), 
    Bias3_tree = c(boot_tree$Bias , b632_tree$Bias, b632_plus_tree$Bias, cv_tree$Bias, loocv_tree$Bias, bcv_tree$Bias, mccv50_tree$Bias),
    MSE4_lda = c(boot_lda$MSE , b632_lda$MSE, b632_plus_lda$MSE, cv_lda$MSE, loocv_lda$MSE, bcv_lda$MSE, mccv50_lda$MSE), 
    Bias4_lda = c(boot_lda$Bias , b632_lda$Bias, b632_plus_lda$Bias, cv_lda$Bias, loocv_lda$Bias, bcv_lda$Bias, mccv50_lda$Bias),
    MSE3_qda = c(boot_qda$MSE , b632_qda$MSE, b632_plus_qda$MSE, cv_qda$MSE, loocv_qda$MSE, bcv_qda$MSE, mccv50_qda$MSE), 
    Bias3_qda = c(boot_qda$Bias , b632_qda$Bias, b632_plus_qda$Bias, cv_qda$Bias, loocv_qda$Bias, bcv_qda$Bias, mccv50_qda$Bias)
  )
  
  data <- data %>%
    mutate(across(starts_with("MSE"), sqrt))
  rownames(data) <- c("boot", ".632", ".632+", "10-fold CV", "LOOCV", "BCV", "MC-CV(50)")
  data <- cbind(Row = rownames(data), data)
  
  # colonne
  colnames(data) <- c("Stimatore", rep(c("RMSE", "Bias"), 5))
  
  # tabella
  kable(data, "html", row.names = FALSE) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    add_header_above(c(" " = 1, "5-NN" = 2, "7-NN" = 2, "TREE" = 2, "LDA" = 2, "QDA" = 2))  %>%
    kable_styling(bootstrap_options = c("hover", "bordered", "striped", "responsive"))
  
}


# tabella_heart <- tabella_confronti(heart_statlog_cleveland_hungary_final, classe = 12, R = 30, seed = 123)
tabella_heart

# tabella_diabetes <- tabella_confronti(diabetes, classe = 9, R = 30, seed = 123)
tabella_diabetes

# tabella_iris <- tabella_confronti(dataset = iris, classe = 5, R = 50, seed = 123)
tabella_iris










































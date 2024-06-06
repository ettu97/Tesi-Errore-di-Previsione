

#### TABELLA PER CONFRONTI ####

tabella_confronti <- function(dataset, classe, R = NULL, seed = NULL,...) {
  #### KNN K=3 ####
  MSE_boot_3nn <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$MSE
  MSE_632_3nn <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$MSE
  MSE_632_plus_3nn <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$MSE
  MSE_cv_3nn <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$MSE
  
  BIAS_boot_3nn <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$Bias
  BIAS_632_3nn <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$Bias
  BIAS_632_plus_3nn <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$Bias
  BIAS_cv_3nn <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_3nn, classify = classify_knn, seed = seed)$Bias
  
  #### KNN K=7 ####
  MSE_boot_7nn <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$MSE
  MSE_632_7nn <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$MSE
  MSE_632_plus_7nn <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$MSE
  MSE_cv_7nn <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$MSE
  
  BIAS_boot_7nn <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$Bias
  BIAS_632_7nn <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$Bias
  BIAS_632_plus_7nn <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$Bias
  BIAS_cv_7nn <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_7nn, classify = classify_knn, seed = seed)$Bias
  
  #### LDA ####
  MSE_boot_lda <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$MSE
  MSE_632_lda <- MSE_BIAS_632(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$MSE
  MSE_632_plus_lda <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$MSE
  MSE_cv_lda <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$MSE
  
  BIAS_boot_lda <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$Bias
  BIAS_632_lda <- MSE_BIAS_632(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$Bias
  BIAS_632_plus_lda <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$Bias
  BIAS_cv_lda <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = MASS:::lda , classify = lda_wrapper, seed = seed)$Bias
  
  #### QDA ####
  MSE_boot_qda <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$MSE
  MSE_632_qda <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$MSE
  MSE_632_plus_qda <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$MSE
  MSE_cv_qda <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$MSE
  
  BIAS_boot_qda <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$Bias
  BIAS_632_qda <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$Bias
  BIAS_632_plus_qda <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$Bias
  BIAS_cv_qda <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_qda , classify = classify_qda, seed = seed)$Bias
  
  #### TREE ####
  MSE_boot_tree <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$MSE
  MSE_632_tree <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$MSE
  MSE_632_plus_tree <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$MSE
  MSE_cv_tree <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$MSE
  
  BIAS_boot_tree <- MSE_BIAS_boot(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$Bias
  BIAS_632_tree <- MSE_BIAS_632(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$Bias
  BIAS_632_plus_tree <- MSE_BIAS_632_plus(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$Bias
  BIAS_cv_tree <- MSE_BIAS_CV(dataset, classe = classe, R = R, train = train_tree, classify = classify_tree, seed = seed)$Bias
  
  # Create the data frame
  data <- data.frame(
    MSE1_3nn = c(MSE_boot_3nn , MSE_632_3nn, MSE_632_plus_3nn, MSE_cv_3nn), 
    Bias1_3nn = c(BIAS_boot_3nn , BIAS_632_3nn, BIAS_632_plus_3nn, BIAS_cv_3nn),
    MSE2_7nn = c(MSE_boot_7nn , MSE_632_7nn, MSE_632_plus_7nn, MSE_cv_7nn), 
    Bias2_7nn = c(BIAS_boot_7nn , BIAS_632_7nn, BIAS_632_plus_7nn, BIAS_cv_7nn),
    MSE3_tree = c(MSE_boot_tree , MSE_632_tree, MSE_632_plus_tree, MSE_cv_tree), 
    Bias3_tree = c(BIAS_boot_tree , BIAS_632_tree, BIAS_632_plus_tree, BIAS_cv_tree),
    MSE4_lda = c(MSE_boot_lda , MSE_632_lda, MSE_632_plus_lda, MSE_cv_lda), 
    Bias4_lda = c(BIAS_boot_lda , BIAS_632_lda, BIAS_632_plus_lda, BIAS_cv_lda),
    MSE5_qda = c(MSE_boot_qda , MSE_632_qda, MSE_632_plus_qda, MSE_cv_qda), 
    Bias5_qda = c(BIAS_boot_qda , BIAS_632_qda, BIAS_632_plus_qda, BIAS_cv_qda)
  )
  
  
  data <- data %>%
    mutate(across(starts_with("MSE"), sqrt))
  rownames(data) <- c("boot", ".632", ".632+", "10-fold CV")
  data <- cbind(Row = rownames(data), data)
  
  # Rename the columns
  colnames(data) <- c("Stimatore", rep(c("RMSE", "Bias"), 5))
  
  # Create the kable table and add headers
  kable(data, "html", row.names = FALSE) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    add_header_above(c(" " = 1, "3-NN" = 2, "7-NN" = 2, "TREE" = 2, "LDA" = 2, "QDA" = 2))  %>%
    kable_styling(bootstrap_options = c("hover", "bordered", "striped", "responsive"))
 
}


# tabella_heart <- tabella_confronti(heart_statlog_cleveland_hungary_final, classe = 12, R = 50, seed = 123)
tabella_heart

# tabella_diabetes <- tabella_confronti(diabetes, classe = 9, R = 50, seed = 123)
tabella_diabetes

# tabella_iris <- tabella_confronti(iris, classe = 5, R = 50, seed = 123)
tabella_iris













































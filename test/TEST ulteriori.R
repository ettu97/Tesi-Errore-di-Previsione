


dataset <- normalize(heart_statlog_cleveland_hungary_final) %>% mutate(sex=as.factor(sex), 
                                                                       chest.pain.type=as.factor(chest.pain.type),
                                                                       fasting.blood.sugar=as.factor(fasting.blood.sugar),
                                                                       resting.ecg=as.factor(resting.ecg),
                                                                       exercise.angina=as.factor(exercise.angina),
                                                                       ST.slope=as.factor(ST.slope),
                                                                       target=as.factor(target))

skim(dataset)
table(dataset$sex)
table(dataset$chest.pain.type)
table(dataset$fasting.blood.sugar)
table(dataset$resting.ecg)
table(dataset$exercise.angina)
table(dataset$ST.slope)
table(dataset$target)

set.seed(415)
idx = sample(nrow(dataset), nrow(dataset)*0.7) 
# Training Set
trn = dataset[idx, ]
# Test Set
tst = dataset[-idx,] 

classe <- as.numeric(classe)

# trasformazioni necessarie
x <- data.matrix(trn[, -classe])
y <- trn[, classe]
z <- data.matrix(tst[, -classe])
w <- tst[, classe]


set.seed(123)
pred_knn <-  knn(train = trn[,-12], test = tst[,-12], cl = trn[,12],k = 9, prob = TRUE)
(conf_matrix<-confusionMatrix(as.factor(pred_knn),tst$target))




errorest_loocv(x, y, z, w, train=train_9nn, classify=classify_knn, seed=123)

###### PROVA #########

#### PARTIZIONE LOOCV ####
loocv_partition <- function(y, seed = NULL) {
  n <- length(y)
  
  # seed per ottenere gli stessi fold
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  folds <- lapply(seq_len(n), function(i) {
    list(
      training = setdiff(seq_len(n), i),
      test = i
    )
  })
  names(folds) <- paste0("Fold", seq_len(n))
  folds
}

#### PARTIZIONE CV #### hold_out=5
cv_partition <- function(y, num_folds = 10, hold_out = NULL, seed = NULL) {
  
  # numero di folds
  n <- length(y)
  if (!is.null(hold_out)) {
    hold_out <- as.integer(hold_out)
    num_folds <- ceiling(n / hold_out)
  } else if (num_folds > n) {
    num_folds <- n
  }
  
  # seed per ottenere gli stessi fold
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # costruzione dei folds
  folds <- split(sample(seq_len(n), n), gl(n = num_folds, k = 1, length = n))
  folds <- lapply(folds, function(fold) {
    list(
      training = which(!seq_along(y) %in% fold),
      test = fold
    )
  })
  names(folds) <- paste0("Fold", names(folds))
  folds
}

#### CALCOLO ERRORE APPARENTE (TRAINING SET) ####
errorest_apparent <- function(x, y, train, classify, ...) {
  # trasformazioni necessarie
  x <- as.matrix(x)
  y <- as.factor(y)
  
  # errore apparente
  train_out <- train(x, y, ...)
  classifications <- classify(object = train_out, newdata = x)
  mean(y != classifications)
}

#### CALCOLO ERRORE LOO BOOTSTRAP ####
errorest_loo_boot <- function(x, y, train, classify, num_bootstraps = 50, ...) {
  # trasformazioni necessarie
  x <- as.matrix(x)
  y <- as.factor(y)
  
  # elementi ausiliari
  seq_y <- seq_along(y)
  rep_NA <- rep.int(NA, times = length(y))
  
  # campionamento con reinserimento, creando un train e test (1 osservazione LOO)
  loo_boot_error_rates <- lapply(seq_len(num_bootstraps), function(b) {
    training <- sample(seq_y, replace = TRUE)
    test <- which(!(seq_y %in% training))
    train_out <- train(x[training, ], y[training])
    # calcolo misclassificate per ogni campione bootstrap sull'osservazione non presente in esso
    classifications <- classify(object = train_out, newdata = x[test, ])
    replace(rep_NA, test, classifications != y[test])
  })
  loo_boot_error_rates <- do.call(rbind, loo_boot_error_rates)
  loo_boot_error_rates <- colMeans(loo_boot_error_rates, na.rm = TRUE)
  
  # errore LOO
  mean(loo_boot_error_rates)
}

#### PARTIZIONE CV #### hold_out=5
bcv_partition <- function(y, num_folds = 10, hold_out = NULL, seed = NULL) {
  
  # numero di folds
  n <- length(y)
  if (!is.null(hold_out)) {
    hold_out <- as.integer(hold_out)
    num_folds <- ceiling(n / hold_out)
  } else if (num_folds > n) {
    num_folds <- n
  }
  
  # seed per ottenere gli stessi fold
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # costruzione dei folds
  folds <- split(sample(seq_len(n), n), gl(n = num_folds, k = 1, length = n))
  folds <- lapply(folds, function(fold) {
    list(
      training = which(!seq_along(y) %in% fold),
      test = fold
    )
  })
  names(folds) <- paste0("Fold", names(folds))
  folds
}

#### CALCOLO ERRORE CV ####
errorest_cv_BCV <- function(x, y, train, classify, num_folds = 10, hold_out = NULL, ...) {
  x <- as.matrix(x)
  y <- as.factor(y)
  
  list_partitions <- bcv_partition(y, num_folds = num_folds, hold_out = hold_out)
  
  # calcolo misclassificate
  num_misclassified <- sapply(list_partitions, function(fold) {
    train_out <- with(fold, train(x[training, ], y[training], ...))
    classifications <- with(fold, classify(object = train_out,
                                           newdata = x[-training, ]))
    with(fold, sum(classifications != y[-training]))
  })
  # errore CV
  sum(num_misclassified) / length(y)
  
}



#### CALCOLO ERRORE BOOTSTRAP .632 ####
errorest <- function(x, y, z, w, train, classify, num_bootstraps = 50, train_size = 0.7,
                     num_folds = 10, hold_out = NULL, num_splits = 50, seed = NULL , ...) {
  # trasformazioni necessarie
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
  # seed per replicabilità 
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  
  # campionamento con reinserimento, creando un train e test
  boot_error_rates <- sapply(seq_len(num_bootstraps), function(b) {
    training <- sample(seq_along(y), replace = TRUE)
    train_out <- train(x[training, ], y[training], ...)
    # calcolo misclassificate per ogni campione bootstrap
    classifications <- classify(object = train_out, newdata = x)
    mean(classifications != y)
  })
  
  # errore boot
  boot_err <- mean(boot_error_rates)
  
  # seed per replicabilità 
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  # calcolo errore apparente
  apparent <- errorest_apparent(x = x, y = y, train = train, classify = classify, ...)
  
  set.seed(seed) # seed di nuovo dopo calcolo errore apparente
  # calcolo errore LOO
  loo_boot <- errorest_loo_boot(x = x, y = y, train = train, classify = classify,
                                num_bootstraps = num_bootstraps, ...)
  
  # errore LOOB
  loob_err <- loo_boot
  
  # errore .632
  boot_err_632 <- 0.368 * apparent + 0.632 * loo_boot
  
  # seed per replicabilità 
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  
  # costruzione stimatore 'no-information error rate' (gamma_hat)
  train_out <- train(x, y, ...)
  classify_out <- classify(object = train_out, newdata = x)
  
  n <- length(y)
  p_k <- as.vector(table(y)) / n
  q_k <- as.vector(table(classify_out)) / n
  gamma_hat <- drop(p_k %*% (1 - q_k))
  
  # stimatore per il 'relative overfitting rate'
  R_hat <- (loo_boot - apparent) / (gamma_hat - apparent)
  
  # calcolo del peso w_hat per il .632+
  w_hat <- 0.632 / (1 - 0.368 * R_hat)
  
  # errore .632+
  boot_err_632_plus <- (1 - w_hat) * apparent + w_hat * loo_boot
  
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  
  list_partitions <- loocv_partition(y, seed = seed)
  # calcolo misclassificate
  num_misclassified <- sapply(list_partitions, function(fold) {
    train_out <- with(fold, train(x[training, ], y[training], ...))
    classifications <- with(fold, classify(object = train_out,
                                           newdata = x[test, , drop = FALSE]))
    with(fold, sum(classifications != y[test]))
  })
  # errore LOOCV
  err_loocv <- sum(num_misclassified) / length(y)
  
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  set.seed(seed)
  list_partitions <- cv_partition(y, num_folds = num_folds, hold_out = hold_out, seed = seed)
  
  # Calculate misclassified errors for each fold
  errors <- sapply(list_partitions, function(fold) {
    train_out <- with(fold, train(x[training, ], y[training], ...))
    classifications <- with(fold, classify(object = train_out, newdata = x[test, ]))
    with(fold, mean(classifications != y[test]))  # Average error for this fold
  })
  
  # CV error (average over all folds)
  err_cv <- mean(errors)
  
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  
  # calcolo errore bcv
  bcv_error_rates <- sapply(seq_len(num_bootstraps), function(b) {
    training <- sample(seq_along(y), replace = TRUE)
    errorest_cv_BCV(x = x[training, ], y = y[training], train = train,
                    classify = classify, num_folds = num_folds,
                    hold_out = hold_out, ...)
    
  })
  
  # errore bcv
  err_bcv <- mean(bcv_error_rates)
  
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  
  # numero oss
  n <- length(y)
  
  # number di training samples
  n_train <- floor(train_size * n)
  
  # vettori errori
  errors_mccv <- numeric(num_splits)
  
  #  Monte Carlo Cross-Validation
  for (i in 1:num_splits) {
    train_indices <- sample(seq_len(n), n_train)
    test_indices <- setdiff(seq_len(n), train_indices)
    train_out <- train(x[train_indices, ], y[train_indices], ...)
    classifications <- classify(object = train_out, newdata = x[test_indices, ])
    errors_mccv[i] <- mean(classifications != y[test_indices])
  }
  
  # errore MCCV 
  err_mccv <- mean(errors_mccv)
  
  
  # calcolo errore su Test (w+z)
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  model <- train(x, y)
  predictions <- classify(model, z)
  true_err <- mean(predictions != w)
  
  # restituzione risultati
  result_list <- list(true_err, boot_err_632, err_cv, err_bcv, boot_err_632_plus,
                      boot_err, loob_err, err_loocv, err_mccv)
  return(result_list)
}

errorest(x, y, z, w, train=train_9nn, classify=classify_knn, seed=123)


#### CALCOLO MSE E BIAS ####
MSE_BIAS <- function(dataset, classe, train, classify, num_bootstraps = 100, num_folds = 10,
                     hold_out = NULL, num_splits = 50, train_size = 0.7, seed = NULL , R = NULL , ...) {
  differenze_632_MSE <- numeric(0)
  differenze_632_BIAS <- numeric(0)
  differenze_cv_MSE <- numeric(0)
  differenze_cv_BIAS <- numeric(0)
  differenze_bcv_MSE <- numeric(0)
  differenze_bcv_BIAS <- numeric(0)
  differenze_632plus_MSE <- numeric(0)
  differenze_632plus_BIAS <- numeric(0)
  differenze_boot_MSE <- numeric(0)
  differenze_boot_BIAS <- numeric(0)
  differenze_loob_MSE <- numeric(0)
  differenze_loob_BIAS <- numeric(0)
  differenze_loocv_MSE <- numeric(0)
  differenze_loocv_BIAS <- numeric(0)
  differenze_mccv_MSE <- numeric(0)
  differenze_mccv_BIAS <- numeric(0)
  
  
  boot632_errs <- numeric(R)
  cv_errs <- numeric(R)
  bcv_errs <- numeric(R)
  boot632plus_errs <- numeric(R)
  boot_errs <- numeric(R)
  loob_errs <- numeric(R)
  loocv_errs <- numeric(R)
  mccv_errs <- numeric(R)
  
  true_errs <- numeric(R)
  
  # in caso R non venisse specificato
  if (is.null(R)) {R <- 50}
  # in caso il seed non venisse specificato
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  set.seed(seed)
  # seed per replicabilità
  seeds <- c(sample.int(1000, R))
  set.seed(seed)
  # calcolo errori per ogni ripetizione R
  for (r in 1:R) {
    set.seed(seeds[r])
    seed <- seeds[r]
    # Suddivisione
    idx = sample(nrow(dataset), 200) 
    # Training Set
    trn = dataset[idx, ]
    # Test Set
    tst = dataset[-idx,] 
    classe <- as.numeric(classe)

    # trasformazioni necessarie
    x <- data.matrix(trn[, -classe])
    y <- trn[, classe]
    z <- data.matrix(tst[, -classe])
    w <- tst[, classe]
    # errori
    errori <- errorest(x, y, z, w, train, classify, num_bootstraps = 50, 
                       num_folds = 10, hold_out = NULL, num_splits = 50, seed = seeds[r], ...)
    true_errs[r] <- errori[[1]]
    boot632_errs[r] <- errori[[2]]
    cv_errs[r] <- errori[[3]]
    bcv_errs[r] <- errori[[4]]
    boot632plus_errs[r] <- errori[[5]]
    boot_errs[r] <- errori[[6]]
    loob_errs[r] <- errori[[7]]
    loocv_errs[r] <- errori[[8]]
    mccv_errs[r] <- errori[[9]]
    
    
    # (.632) errore quadratico per la r-esima ripetizione
    differenza_632_cubo <- (errori[[2]]-errori[[1]])^2
    differenze_632_MSE <- c(differenze_632_MSE, differenza_632_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_632 <- (errori[[2]]-errori[[1]])
    differenze_632_BIAS <- c(differenze_632_BIAS, differenza_632)
    
    # (CV) errore quadratico per la r-esima ripetizione
    differenza_cv_cubo <- (errori[[3]]-errori[[1]])^2
    differenze_cv_MSE <- c(differenze_cv_MSE, differenza_cv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_cv <- (errori[[3]]-errori[[1]])
    differenze_cv_BIAS <- c(differenze_cv_BIAS, differenza_cv)

    # (BCV) errore quadratico per la r-esima ripetizione
    differenza_bcv_cubo <- (errori[[4]]-errori[[1]])^2
    differenze_bcv_MSE <- c(differenze_bcv_MSE, differenza_bcv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_bcv <- (errori[[4]]-errori[[1]])
    differenze_bcv_BIAS <- c(differenze_bcv_BIAS, differenza_bcv)
    
    # (.632plus) errore quadratico per la r-esima ripetizione
    differenza_632plus_cubo <- (errori[[5]]-errori[[1]])^2
    differenze_632plus_MSE <- c(differenze_632plus_MSE, differenza_632plus_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_632plus <- (errori[[5]]-errori[[1]])
    differenze_632plus_BIAS <- c(differenze_632plus_BIAS, differenza_632plus)
    
    # (BOOT) errore quadratico per la r-esima ripetizione
    differenza_boot_cubo <- (errori[[6]]-errori[[1]])^2
    differenze_boot_MSE <- c(differenze_boot_MSE, differenza_boot_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_boot <- (errori[[6]]-errori[[1]])
    differenze_boot_BIAS <- c(differenze_boot_BIAS, differenza_boot)
    
    # (LOOB) errore quadratico per la r-esima ripetizione
    differenza_loob_cubo <- (errori[[7]]-errori[[1]])^2
    differenze_loob_MSE <- c(differenze_loob_MSE, differenza_loob_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_loob <- (errori[[7]]-errori[[1]])
    differenze_loob_BIAS <- c(differenze_loob_BIAS, differenza_loob)
    
    # (LOOCV) errore quadratico per la r-esima ripetizione
    differenza_loocv_cubo <- (errori[[8]]-errori[[1]])^2
    differenze_loocv_MSE <- c(differenze_loocv_MSE, differenza_loocv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_loocv <- (errori[[8]]-errori[[1]])
    differenze_loocv_BIAS <- c(differenze_loocv_BIAS, differenza_loocv)
    
    # (MCCV) errore quadratico per la r-esima ripetizione
    differenza_mccv_cubo <- (errori[[9]]-errori[[1]])^2
    differenze_mccv_MSE <- c(differenze_mccv_MSE, differenza_mccv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_mccv <- (errori[[9]]-errori[[1]])
    differenze_mccv_BIAS <- c(differenze_mccv_BIAS, differenza_mccv)
    
    
  }
  # MSE
  MSE_632 <- sum(differenze_632_MSE)/R
  MSE_cv <- sum(differenze_cv_MSE)/R
  MSE_bcv <- sum(differenze_bcv_MSE)/R
  MSE_632plus <- sum(differenze_632plus_MSE)/R
  MSE_boot <- sum(differenze_boot_MSE)/R
  MSE_loob <- sum(differenze_loob_MSE)/R
  MSE_loocv <- sum(differenze_loocv_MSE)/R
  MSE_mccv <- sum(differenze_mccv_MSE)/R
  
  # BIAS
  BIAS_632 <- sum(differenze_632_BIAS)/R
  BIAS_cv <- sum(differenze_cv_BIAS)/R
  BIAS_bcv <- sum(differenze_bcv_BIAS)/R
  BIAS_632plus <- sum(differenze_632plus_BIAS)/R
  BIAS_boot <- sum(differenze_boot_BIAS)/R
  BIAS_loob <- sum(differenze_loob_BIAS)/R
  BIAS_loocv <- sum(differenze_loocv_BIAS)/R
  BIAS_mccv <- sum(differenze_mccv_BIAS)/R
  
  
  
  # restituzione risultati
  result_list <- list(MSE_632 = MSE_632, Bias_632 = BIAS_632, errori_632 = boot632_errs,
                      MSE_cv = MSE_cv, Bias_cv = BIAS_cv,  errori_cv = cv_errs,
                      MSE_bcv = MSE_bcv, Bias_bcv = BIAS_bcv, errori_bcv = bcv_errs,
                      MSE_632plus = MSE_632plus, Bias_632plus = BIAS_632plus, errori_632plus = boot632plus_errs,
                      MSE_boot = MSE_boot, Bias_boot = BIAS_boot, errori_boot = boot_errs,
                      MSE_loob = MSE_loob, Bias_loob = BIAS_loob, errori_loob = loob_errs,
                      MSE_loocv = MSE_loocv, Bias_loocv = BIAS_loocv, errori_loocv = loocv_errs,
                      MSE_mccv = MSE_mccv, Bias_mccv = BIAS_mccv, errori_mccv = mccv_errs,
                      true_errors = true_errs, seeds)
  return(result_list)
}


0.042*1190

######## 9NN########

mod_9nn <- MSE_BIAS(df_norm, classe=17, R=50, train=train_9nn, classify=classify_knn, seed=123)
mod_9nn

### PLOT ###
veri <- as.data.frame(mod_9nn$true_errors)
stimati632 <- as.data.frame(mod_9nn$errori_632)
stimatiCV <- as.data.frame(mod_9nn$errori_CV)
stimatiBCV <- as.data.frame(mod_9nn$errori_BCV)
stimati632plus <- as.data.frame(mod_9nn$errori_632plus)

stimati632$Index <- rownames(stimati632)  # Create an index column
stimati632$Index <- as.numeric(stimati632$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri,stimati632, stimatiCV, stimatiBCV, stimati632plus)


# Crea il plot
q <- ggplot() +
  geom_point(data = veriVSstimati, aes(x = Index, y = mod_9nn$true_errors, color = "True Errors")) +
  
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$errori_632, color = ".632 Estimates")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$errori_CV, color = "CV Estimates")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$errori_BCV, color = "BCV Estimates")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$errori_632plus, color = ".632plus Estimates")) +
  
  geom_hline(aes(yintercept = mean(mod_9nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_9nn$errori_632), color = "Average .632 Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_9nn$errori_CV), color = "Average CV Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_9nn$errori_BCV), color = "Average BCV Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_9nn$errori_632plus), color = "Average .632plus Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs .632 vs CV vc BCV vs .632+ Estimates con 9NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", "Average True Error" = "blue4",
                                "CV Estimates" = "green3",  "Average CV Estimate" = "green4",
                                ".632 Estimates" = "red", "Average .632 Estimate" = "red4",
                                "BCV Estimates" = "pink", "Average BCV Estimate" = "pink4",
                                ".632plus Estimates" = "yellow", "Average .632plus Estimate" = "yellow3")) +
  theme_minimal()

ggplotly(q)



##### ALBERI #######

mod_tree <- MSE_BIAS(df_norm, classe=17, R=50, train=train_tree2, classify=classify_tree2, seed=123)
mod_tree


### PLOT ###
veri <- as.data.frame(mod_tree$true_errors)
stimati632 <- as.data.frame(mod_tree$errori_632)
stimatiCV <- as.data.frame(mod_tree$errori_CV)
stimatiBCV <- as.data.frame(mod_tree$errori_BCV)


stimati632$Index <- rownames(stimati632)  # Create an index column
stimati632$Index <- as.numeric(stimati632$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri,stimati632, stimatiCV, stimatiBCV)


# Crea il plot
q <- ggplot() +
  geom_point(data = veriVSstimati, aes(x = Index, y = mod_tree$true_errors, color = "True Errors")) +
  
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_tree$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_tree$errori_632, color = ".632 Estimates")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_tree$errori_CV, color = "CV Estimates")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_tree$errori_BCV, color = "BCV Estimates")) +
  
  geom_hline(aes(yintercept = mean(mod_tree$true_errors), color = "Average True Error"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_tree$errori_632), color = "Average .632 Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_tree$errori_CV), color = "Average CV Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_tree$errori_BCV), color = "Average BCV Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs .632 vs CV vc BCV Estimates con tree", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", "Average True Error" = "blue4",
                                "CV Estimates" = "green3",  "Average CV Estimate" = "green4",
                                ".632 Estimates" = "red", "Average .632 Estimate" = "red4",
                                "BCV Estimates" = "pink", "Average BCV Estimate" = "pink4")) +
  theme_minimal()

ggplotly(q)




##### dataset description ####

anyDuplicated(heart_statlog_cleveland_hungary_final)

heart_statlog_cleveland_hungary_final <- heart_statlog_cleveland_hungary_final  %>% 
  mutate(sex=as.factor(sex), 
         chest.pain.type=as.factor(chest.pain.type),
         fasting.blood.sugar=as.factor(fasting.blood.sugar),
         resting.ecg=as.factor(resting.ecg),
         exercise.angina=as.factor(exercise.angina),
         ST.slope=as.factor(ST.slope),
         target=as.factor(target))

str(heart_statlog_cleveland_hungary_final)


dummy <- dummyVars("~ sex + chest.pain.type + fasting.blood.sugar + resting.ecg + exercise.angina + ST.slope", 
                   data = heart_statlog_cleveland_hungary_final,  fullRank = TRUE)

df_one_hot <- data.frame(predict(dummy, newdata = heart_statlog_cleveland_hungary_final))

df_non_cat <- heart_statlog_cleveland_hungary_final %>% select(age , resting.bp.s, cholesterol, max.heart.rate, oldpeak, target)

df_final <- cbind(df_one_hot, df_non_cat)

df_norm <- normalize(df_final) %>% mutate(sex.1=as.factor(sex.1), 
                                          chest.pain.type.2=as.factor(chest.pain.type.2),
                                          chest.pain.type.3=as.factor(chest.pain.type.3),
                                          chest.pain.type.4=as.factor(chest.pain.type.4),
                                          fasting.blood.sugar.1=as.factor(fasting.blood.sugar.1),
                                          resting.ecg.1=as.factor(resting.ecg.1),
                                          resting.ecg.2=as.factor(resting.ecg.2),
                                          exercise.angina.1=as.factor(exercise.angina.1),
                                          ST.slope.1=as.factor(ST.slope.1),
                                          ST.slope.2=as.factor(ST.slope.2),
                                          ST.slope.3=as.factor(ST.slope.3))

str(df_norm)

skim(df_norm)

set.seed(123)
idx = sample(nrow(df_norm), nrow(df_norm)*0.7) 
# Training Set
training = df_norm[idx, ]
# Test Set
test = df_norm[-idx,] 

classe <- as.numeric(17)

# trasformazioni necessarie
x_trn <- data.matrix(training[, -classe])
y_trn <- training[, classe]
z_tst <- data.matrix(test[, -classe])
w_tst <- test[, classe]





err_rate = function(actual, predicted) { mean(actual != predicted) }

set.seed(123)
k_to_try = 1:100
err_k = rep(x = 0, times = length(k_to_try))
for (i in seq_along(k_to_try)) {
  pred = knn(train = training[,-17],
             test = test[,-17], cl = training[,17],
             k = k_to_try[i])
  err_k[i] = err_rate(test[,17], pred) }

plot(err_k, type = "b", col = "cornflowerblue", cex = 1, pch = 20,
     xlab = "K", ylab = "Error rate", 
     main = "Error rate vs K")






set.seed(123)
pred_knn <-  knn(train = training[,-17], test = test[,-17], cl = training[,17],k = 9, prob = TRUE)
(conf_matrix<-confusionMatrix(as.factor(pred_knn),test$target))



#### TABELLA 9NN ####


# dataframe
data_9nn <- data.frame(
  MSE2_9nn = c(mod_9nn$MSE_boot , mod_9nn$MSE_loob, mod_9nn$MSE_632, mod_9nn$MSE_632plus, 
               mod_9nn$MSE_loocv, mod_9nn$MSE_cv, mod_9nn$MSE_mccv, mod_9nn$MSE_bcv), 
  Bias2_9nn = c(mod_9nn$Bias_boot , mod_9nn$Bias_loob, mod_9nn$Bias_632, mod_9nn$Bias_632plus, 
                mod_9nn$Bias_loocv, mod_9nn$Bias_cv, mod_9nn$Bias_mccv, mod_9nn$Bias_bcv)
  )

data_9nn <- data_9nn %>%
  mutate(across(starts_with("MSE"), sqrt))
rownames(data_9nn) <- c("BOOT", "LOOB", ".632", ".632+", "LOOCV", "10-fold CV", "MCCV", "BCV")
data_9nn <- cbind(Row = rownames(data_9nn), data_9nn)

# colonne
colnames(data_9nn) <- c("Stimatore", rep(c("RMSE", "Bias"), 1))

# tabella
kable(data_9nn, "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  add_header_above(c(" " = 1, "9-NN" = 2))  %>%
  kable_styling(bootstrap_options = c("hover", "bordered", "striped", "responsive"))


#### TABELLA TREE ####

# dataframe
data_tree <- data.frame(
  MSE2_tree = c(mod_tree$MSE_boot , mod_tree$MSE_loob, mod_tree$MSE_632, mod_tree$MSE_632plus, 
               mod_tree$MSE_loocv, mod_tree$MSE_cv, mod_tree$MSE_mccv, mod_tree$MSE_bcv), 
  Bias2_tree = c(mod_tree$Bias_boot , mod_tree$Bias_loob, mod_tree$Bias_632, mod_tree$Bias_632plus, 
                mod_tree$Bias_loocv, mod_tree$Bias_cv, mod_tree$Bias_mccv, mod_tree$Bias_bcv)
)

data_tree <- data_tree %>%
  mutate(across(starts_with("MSE"), sqrt))
rownames(data_tree) <- c("BOOT", "LOOB", ".632", ".632+", "LOOCV", "10-fold CV", "MCCV", "BCV")
data_tree <- cbind(Row = rownames(data_tree), data_tree)

# colonne
colnames(data_tree) <- c("Stimatore", rep(c("RMSE", "Bias"), 1))

# tabella
kable(data_tree, "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  add_header_above(c(" " = 1, "TREE" = 2))  %>%
  kable_styling(bootstrap_options = c("hover", "bordered", "striped", "responsive"))


### BOXPLOT 9NN ####

errors_df_9nn <- data.frame(
  mod_9nn$errori_boot,
  mod_9nn$errori_loob, 
  mod_9nn$errori_632,
  mod_9nn$errori_632plus,
  mod_9nn$errori_loocv,
  mod_9nn$errori_cv,
  mod_9nn$errori_mccv,
  mod_9nn$errori_bcv
)

media_true_9nn <- mean(mod_9nn$true_errors)


# 
errors_long_9nn <- pivot_longer(errors_df_9nn, 
                            cols = everything(), 
                            names_to = "Method", 
                            values_to = "ErrorRate")

# 
boxplot_9nn <- ggplot(errors_long_9nn, aes(x = Method, y = ErrorRate)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = media_true_9nn, color = "red", linetype = "dashed", size = 1) +
  scale_x_discrete(labels = c("mod_9nn.errori_boot" = "BOOT",
                              "mod_9nn.errori_loob" = "LOOB",
                              "mod_9nn.errori_632" = ".632",
                              "mod_9nn.errori_632plus" = ".632+",
                              "mod_9nn.errori_loocv" = "LOOCV",
                              "mod_9nn.errori_cv" = "10-fold CV",
                              "mod_9nn.errori_mccv" = "MCCV",
                              "mod_9nn.errori_bcv" = "BCV")) +
  labs(title = "Boxplot 9NN",
       y = "Valore",
       x = "Stimatore") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
boxplot_9nn








































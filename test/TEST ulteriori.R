


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
errorest_loo_boot <- function(x, y, train, classify, num_bootstraps = 100, ...) {
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
errorest <- function(x, y, z, w, train, classify, num_bootstraps = 100,
                     num_folds = 10, hold_out = NULL, seed = NULL , ...) {
  # trasformazioni necessarie
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
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
  # errore .632
  boot_err_632 <- 0.368 * apparent + 0.632 * loo_boot
  
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
  
  err_bcv <- mean(bcv_error_rates)
  
  # calcolo errore su Test (w+z)
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  set.seed(seed)
  model <- train(x, y)
  predictions <- classify(model, z)
  true_err <- mean(predictions != w)
  
  # restituzione risultati
  result_list <- list(true_err, boot_err_632, err_cv, err_bcv)
  return(result_list)
}

errorest(x, y, z, w, train=train_9nn, classify=classify_knn, seed=123)


#### CALCOLO MSE E BIAS ####
MSE_BIAS <- function(dataset, classe, train, classify, num_bootstraps = 100,
                         num_folds = 10, hold_out = NULL, seed = NULL , R = NULL , ...) {
  differenze_632_MSE <- numeric(0)
  differenze_632_BIAS <- numeric(0)
  differenze_cv_MSE <- numeric(0)
  differenze_cv_BIAS <- numeric(0)
  differenze_bcv_MSE <- numeric(0)
  differenze_bcv_BIAS <- numeric(0)
  
  boot632_errs <- numeric(R)
  cv_errs <- numeric(R)
  bcv_errs <- numeric(R)
  
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
    idx = sample(nrow(dataset), nrow(dataset)*0.3) 
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
    # errori .632
    errori <- errorest(x, y, z, w, train, classify, num_bootstraps = 100,
                           num_folds = 10, hold_out = NULL, seed = seeds[r], ...)
    true_errs[r] <- errori[[1]]
    boot632_errs[r] <- errori[[2]]
    cv_errs[r] <- errori[[3]]
    bcv_errs[r] <- errori[[4]]
    
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
  
  }
  # MSE
  MSE_632 <- sum(differenze_632_MSE)/R
  MSE_cv <- sum(differenze_cv_MSE)/R
  MSE_bcv <- sum(differenze_bcv_MSE)/R
  # BIAS
  BIAS_632 <- sum(differenze_632_BIAS)/R
  BIAS_cv <- sum(differenze_cv_BIAS)/R
  BIAS_bcv <- sum(differenze_bcv_BIAS)/R
  
  
  # restituzione risultati
  result_list <- list(MSE_632 = MSE_632, Bias_632 = BIAS_632, errori_632 = boot632_errs,
                      MSE_CV = MSE_cv, Bias_CV = BIAS_cv,  errori_CV = cv_errs,
                      MSE = MSE_bcv, Bias = BIAS_bcv, errori_BCV = bcv_errs,
                      true_errors = true_errs, seeds)
  return(result_list)
}




######## 9NN########

mod_9nn <- MSE_BIAS(dataset, classe=12, R=50, train=train_9nn, classify=classify_knn, seed=123)
mod_9nn

### PLOT ###
veri632 <- as.data.frame(mod_9nn$true_errors)
stimati632 <- as.data.frame(mod_9nn$errori_632)
stimatiCV <- as.data.frame(mod_9nn$errori_CV)
stimatiBCV <- as.data.frame(mod_9nn$errori_BCV)


stimati632$Index <- rownames(stimati632)  # Create an index column
stimati632$Index <- as.numeric(stimati632$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri632,stimati632, stimatiCV, stimatiBCV)


# Crea il plot
q <- ggplot() +
  geom_point(data = veriVSstimati, aes(x = Index, y = mod_9nn$true_errors, color = "True Errors")) +
  
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$errori_632, color = ".632 Estimates")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$errori_CV, color = "CV Estimates")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = mod_9nn$errori_BCV, color = "BCV Estimates")) +
  
  geom_hline(aes(yintercept = mean(mod_9nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_9nn$errori_632), color = "Average .632 Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_9nn$errori_CV), color = "Average CV Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(mod_9nn$errori_BCV), color = "Average BCV Estimate"), 
             linetype = "dashed", linewidth = 1, show.legend = TRUE) +

  labs(title = "Plot di True Errors vs .632 vs CV vc BCV Estimates con 9NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", "Average True Error" = "blue4",
                                "CV Estimates" = "green3",  "Average CV Estimate" = "green4",
                                ".632 Estimates" = "red", "Average .632 Estimate" = "red4",
                                "BCV Estimates" = "pink", "Average BCV Estimate" = "pink4")) +
  theme_minimal()

ggplotly(q)



##### ALBERI #######

mod_tree <- MSE_BIAS(dataset, classe=12, R=50, train=train_tree2, classify=classify_tree2, seed=123)
mod_tree


### PLOT ###
veri632 <- as.data.frame(mod_tree$true_errors)
stimati632 <- as.data.frame(mod_tree$errori_632)
stimatiCV <- as.data.frame(mod_tree$errori_CV)
stimatiBCV <- as.data.frame(mod_tree$errori_BCV)


stimati632$Index <- rownames(stimati632)  # Create an index column
stimati632$Index <- as.numeric(stimati632$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri632,stimati632, stimatiCV, stimatiBCV)


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


















































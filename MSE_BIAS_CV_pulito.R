
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

#### CALCOLO ERRORE CV ####
errorest_cv <- function(x, y, z, w, train, classify, num_folds = 10, hold_out = NULL, seed = NULL, ...) {
  # trasformazioni necessarie
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
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
  
  # Calculate true error on Test (w+z)
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  
  # Return results
  result_list <- list(err_cv, true_err)
  names(result_list) <- c("CV Error", "True Error")
  return(result_list)
}

#### CALCOLO MSE E BIAS CV ####
MSE_BIAS_CV <- function(dataset, classe, train, classify, num_folds = 10, 
                        hold_out = NULL, seed = NULL, R = NULL , ...) {
  differenze_cv_MSE <- numeric(0)
  differenze_cv_BIAS <- numeric(0)
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
    # errori CV
    errori_cv <- errorest_cv(x, y, z, w, train, classify, num_folds = 10, 
                             hold_out = NULL, seed = seeds[r], ...)
    # errore quadratico per la r-esima ripetizione
    differenza_cv_cubo <- (errori_cv[[1]]-errori_cv[[2]])^2
    differenze_cv_MSE <- c(differenze_cv_MSE, differenza_cv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_cv <- (errori_cv[[1]]-errori_cv[[2]])
    differenze_cv_BIAS <- c(differenze_cv_BIAS, differenza_cv)
  }
  # MSE
  MSE_cv <- sum(differenze_cv_MSE)/R
  # BIAS
  BIAS_cv <- sum(differenze_cv_BIAS)/R
  # restituzione risultati
  result_list <- list(MSE = MSE_cv, Bias = BIAS_cv)
  return(result_list)
}

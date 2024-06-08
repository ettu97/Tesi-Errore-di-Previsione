
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

#### CALCOLO ERRORE loocv ####
errorest_loocv <- function(x, y, z, w, train, classify, seed = NULL, ...) {
  # trasformazioni necessarie
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
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
  # calcolo errore su Test (w+z)
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  # restituzione risultati
  result_list <- list(err_loocv, true_err)
  return(result_list)
}

#### CALCOLO MSE E BIAS loocv ####
MSE_BIAS_LOOCV <- function(dataset, classe, train, classify, seed = NULL, R = NULL , ...) {
  differenze_loocv_MSE <- numeric(0)
  differenze_loocv_BIAS <- numeric(0)
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
    # errori loocv
    errori_loocv <- errorest_loocv(x, y, z, w, train, classify, seed = seeds[r], ...)
    # errore quadratico per la r-esima ripetizione
    differenza_loocv_cubo <- (errori_loocv[[1]]-errori_loocv[[2]])^2
    differenze_loocv_MSE <- c(differenze_loocv_MSE, differenza_loocv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_loocv <- (errori_loocv[[1]]-errori_loocv[[2]])
    differenze_loocv_BIAS <- c(differenze_loocv_BIAS, differenza_loocv)
  }
  # MSE
  MSE_loocv <- sum(differenze_loocv_MSE)/R
  # BIAS
  BIAS_loocv <- sum(differenze_loocv_BIAS)/R
  # restituzione risultati
  result_list <- list(MSE = MSE_loocv, Bias = BIAS_loocv)
  return(result_list)
}

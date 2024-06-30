
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

#### CALCOLO ERRORE BCV ####
errorest_bcv <- function(x, y, z, w, train, classify, num_bootstraps = 100,
                         num_folds = 10, hold_out = NULL, seed=NULL, ...) {
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
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
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  # restituzione risultati
  result_list <- list(err_bcv, true_err)
  return(result_list)
  
}

#### CALCOLO MSE E BIAS BCV ####
MSE_BIAS_BCV <- function(dataset, classe, train, classify, num_bootstraps = 100,
                         num_folds = 10, hold_out = NULL, seed=NULL, R = NULL , ...) {
  differenze_bcv_MSE <- numeric(0)
  differenze_bcv_BIAS <- numeric(0)
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
    errori_bcv <- errorest_bcv(x, y, z, w, train, classify, num_bootstraps = num_bootstraps,
                               num_folds = 10, hold_out = NULL, seed = seeds[r], ...)
    bcv_errs[r] <- errori_bcv[[1]]
    true_errs[r] <- errori_bcv[[2]]
    # errore quadratico per la r-esima ripetizione
    differenza_bcv_cubo <- (errori_bcv[[1]]-errori_bcv[[2]])^2
    differenze_bcv_MSE <- c(differenze_bcv_MSE, differenza_bcv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_bcv <- (errori_bcv[[1]]-errori_bcv[[2]])
    differenze_bcv_BIAS <- c(differenze_bcv_BIAS, differenza_bcv)
  }
  # MSE
  MSE_bcv <- sum(differenze_bcv_MSE)/R
  # BIAS
  BIAS_bcv <- sum(differenze_bcv_BIAS)/R
  # restituzione risultati
  result_list <- list(MSE = MSE_bcv, Bias = BIAS_bcv, errori = bcv_errs, true_errors = true_errs)
  return(result_list)
}

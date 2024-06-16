
#### PARTIZIONE MCCV ####
mccv_partition <- function(y, train_size = 0.7, num_repeats = NULL, seed = NULL) {
  n <- length(y)
  n_train <- ceiling(n * train_size)
  
  # seed per ottenere gli stessi folds
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # costruzione delle partizioni MCCV
  partitions <- vector("list", num_repeats)
  for (i in 1:num_repeats) {
    train_indices <- sample(seq_len(n), n_train)
    test_indices <- setdiff(seq_len(n), train_indices)
    partitions[[i]] <- list(
      training = train_indices,
      test = test_indices
    )
  }
  names(partitions) <- paste0("Repeat", seq_len(num_repeats))
  partitions
}

#### CALCOLO ERRORE MCCV ####
errorest_mccv <- function(x, y, z, w, train, classify, train_size = 0.7, num_repeats = NULL, seed = NULL, ...) {
  # trasformazioni necessarie
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  set.seed(seed)
  list_partitions <- mccv_partition(y, train_size = train_size, num_repeats = num_repeats, seed = seed)
  
  # calcolo misclassificate
  num_misclassified <- sapply(list_partitions, function(fold) {
    train_out <- with(fold, train(x[training, ], y[training], ...))
    classifications <- with(fold, classify(object = train_out, newdata = x[test, ]))
    with(fold, sum(classifications != y[test]))
  })
  
  # errore MCCV
  err_mccv <- sum(num_misclassified) / (num_repeats * length(y))
  
  # calcolo errore su Test (w+z)
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  
  # restituzione risultati
  result_list <- list(err_mccv, true_err)
  return(result_list)
}

#### CALCOLO MSE E BIAS MC-CV ####
MSE_BIAS_MCCV_100 <- function(dataset, classe, train, classify, num_repeats = 100, seed = NULL, R = NULL , ...) {
  differenze_mccv_MSE <- numeric(0)
  differenze_mccv_BIAS <- numeric(0)
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
    # errori MC-CV
    errori_mccv <- errorest_mccv(x, y, z, w, train, classify, train_size = 0.7, num_repeats = num_repeats, seed = seeds[r])
    mccv_errs[r] <- errori_mccv[[1]]
    true_errs[r] <- errori_mccv[[2]]
    # errore quadratico per la r-esima ripetizione
    differenza_mccv_cubo <- (errori_mccv[[1]]-errori_mccv[[2]])^2
    differenze_mccv_MSE <- c(differenze_mccv_MSE, differenza_mccv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_mccv <- (errori_mccv[[1]]-errori_mccv[[2]])
    differenze_mccv_BIAS <- c(differenze_mccv_BIAS, differenza_mccv)
  }
  # MSE
  MSE_mccv <- sum(differenze_mccv_MSE)/R
  # BIAS
  BIAS_mccv <- sum(differenze_mccv_BIAS)/R
  # restituzione risultati
  result_list <- list(MSE = MSE_mccv, Bias = BIAS_mccv, errori = mccv_errs, true_errors = mean(true_errs))
  return(result_list)
}

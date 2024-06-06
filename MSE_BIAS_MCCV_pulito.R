
#### PARTIZIONE MC-CV #### num_spluts=10
mc_cv_partition <- function(y, num_splits = 10, seed = NULL) {
  n <- length(y)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  splits <- replicate(num_splits, sample(seq_len(n), size = round(0.7 * n)), simplify = FALSE)
  splits <- lapply(splits, function(split) {
    list(
      training = split,
      test = setdiff(seq_len(n), split)
    )
  })
  names(splits) <- paste0("Split", seq_len(num_splits))
  splits
}

#### CALCOLO ERRORE MC-CV ####
errorest_mc_cv <- function(x, y, z, w, train, classify, num_splits = 10, seed = NULL, ...) {
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
  # seed
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  # seed_used <- seed # in caso si volesse sapere il seed utilizzato
  list_partitions <- mc_cv_partition(y, num_splits = num_splits, seed = seed)
  
  num_misclassified <- sapply(list_partitions, function(split) {
    train_out <- with(split, train(x[training, ], y[training], ...))
    classifications <- with(split, classify(object = train_out, newdata = x[test, ]))
    with(split, sum(classifications != y[test]))
  })
  
  # errore MC-CV
  err_mccv <- sum(num_misclassified) / length(y)
  # calcolo errore su Test (w+z)
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  # restituzione risultati
  result_list <- list(err_mccv, true_err)
  return(result_list)
  
}

#### CALCOLO MSE E BIAS MC-CV ####
MSE_BIAS_MCCV <- function(dataset, classe, train, classify, num_splits = 10,
                          seed = NULL, R = NULL , ...) {
  differenze_mccv_MSE <- numeric(0)
  differenze_mccv_BIAS <- numeric(0)
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
    errori_mccv <- errorest_mc_cv(x, y, z, w, train, classify, num_splits = 10, seed = seeds[r], ...)
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
  result_list <- list(MSE = MSE_mccv, Bias = BIAS_mccv)
  return(result_list)
}

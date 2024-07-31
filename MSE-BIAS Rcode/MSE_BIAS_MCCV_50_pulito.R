
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
MSE_BIAS_MCCV_50 <- function(dataset, classe, train, classify, num_repeats = 50, seed = NULL, R = NULL , ...) {
  differenze_mccv_MSE <- numeric(0)
  differenze_mccv_BIAS <- numeric(0)
  mccv_errs <- numeric(R)
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
  result_list <- list(MSE = MSE_mccv, Bias = BIAS_mccv, errori = mccv_errs)
  return(result_list)
}




# ERROREST_MCCV FUNCTION
errorest_mccv2 <- function(x, y, z, w, train, classify, train_size = 0.7, num_repeats = 100, seed = NULL, ...) {
  # Necessary transformations
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
  # Seed for reproducibility
  if (is.null(seed)) {seed <- sample.int(1000, 1)}
  set.seed(seed)
  
  # Partition the data using MCCV
  list_partitions <- mccv_partition(y, train_size = train_size, num_repeats = num_repeats, seed = seed)
  
  # Calculate misclassification errors for each partition
  num_misclassified <- sapply(list_partitions, function(fold) {
    train_out <- with(fold, train(x[training, ], y[training], ...))
    classifications <- with(fold, classify(object = train_out, newdata = x[test, ]))
    with(fold, sum(classifications != y[test]))
  })
  
  # Calculate MCCV error
  err_mccv <- sum(num_misclassified) / (num_repeats * length(y))
  
  # Calculate true error on the separate test set
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  
  # Return results
  result_list <- list(err_mccv, true_err)
  return(result_list)
}

#### CALCOLO MSE E BIAS MC-CV ####
MSE_BIAS_MCCV_50_2 <- function(dataset, classe, train, classify, num_repeats = 50, seed = NULL, R = NULL , ...) {
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
    errori_mccv <- errorest_mccv2(x, y, z, w, train, classify, train_size = 0.7, num_repeats = num_repeats, seed = seeds[r])
    mccv_errs[r] <- errori_mccv[[1]]
    true_errs[r] <- errori_mccv[[2]]
    # errore quadratico per la r-esima ripetizione
#    differenza_mccv_cubo <- (errori_mccv[[1]]-errori_mccv[[2]])^2
#    differenze_mccv_MSE <- c(differenze_mccv_MSE, differenza_mccv_cubo)
    # errore semplice per la r-esima ripetizione
#    differenza_mccv <- (errori_mccv[[1]]-errori_mccv[[2]])
#    differenze_mccv_BIAS <- c(differenze_mccv_BIAS, differenza_mccv)
    
    
    # errore quadratico per la r-esima ripetizione
    differenza_mccv_cubo <- (mccv_errs[r]-true_errs[r])^2
    differenze_mccv_MSE <- c(differenze_mccv_MSE, differenza_mccv_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_mccv <- (mccv_errs[r]-true_errs[r])
    differenze_mccv_BIAS <- c(differenze_mccv_BIAS, differenza_mccv)
  }
  # MSE
  MSE_mccv <- sum(differenze_mccv_MSE)/R
  # BIAS
  BIAS_mccv <- sum(differenze_mccv_BIAS)/R
  # restituzione risultati
  result_list <- list(MSE = MSE_mccv, Bias = BIAS_mccv, errori = mccv_errs, true_errors = true_errs)
  return(result_list)
}


MSE_BIAS_MCCV_50_2(df_norm, classe=17, R=5, train=train_9nn, classify=classify_knn, seed=123)
MSE_BIAS_MCCV_50(df_norm, classe=17, R=5, train=train_9nn, classify=classify_knn, seed=123)



errorest_mccv <- function(x, y, z, w, train, classify, num_splits = 100, train_size = 0.7, seed = NULL, ...) {
  # Transformations
  x <- as.matrix(x)
  y <- as.factor(y)
  z <- as.matrix(z)
  w <- as.factor(w)
  
  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Number of observations
  n <- length(y)
  
  # Number of training samples
  n_train <- floor(train_size * n)
  
  # Initialize errors vector
  errors <- numeric(num_splits)
  
  # Perform Monte Carlo Cross-Validation
  for (i in 1:num_splits) {
    # Randomly split the data into training and test sets
    train_indices <- sample(seq_len(n), n_train)
    test_indices <- setdiff(seq_len(n), train_indices)
    
    # Train the model
    train_out <- train(x[train_indices, ], y[train_indices], ...)
    
    # Classify the test set
    classifications <- classify(object = train_out, newdata = x[test_indices, ])
    
    # Calculate error for this split
    errors[i] <- mean(classifications != y[test_indices])
  }
  
  # MCCV error (average over all splits)
  err_mccv <- mean(errors)
  
  # Calculate true error on Test (w+z)
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  
  # Return results
  result_list <- list(err_mccv, true_err)
  return(result_list)
}







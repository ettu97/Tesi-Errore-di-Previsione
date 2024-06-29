
#### CALCOLO ERRORE BOOT ####
errorest_boot <- function(x, y, z, w, train, classify, num_bootstraps = 100, seed = NULL, ...) {
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
  # calcolo errore su Test (w+z)
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  # restituzione risultati
  result_list <- list(boot_err, true_err)
  return(result_list)
}

#### CALCOLO MSE E BIAS BOOT ####
MSE_BIAS_boot <- function(dataset, classe, train, classify, num_bootstraps = 100,
                          apparent = NULL, loo_boot = NULL, seed = NULL , R = NULL , ...) {
  differenze_boot_MSE <- numeric(0)
  differenze_boot_BIAS <- numeric(0)
  boot_errs <- numeric(R)
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
    # errori boot
    errori_boot <- errorest_boot(x, y, z, w, train, classify, num_bootstraps = num_bootstraps, seed = seeds[r], ...)
    boot_errs[r] <- errori_boot[[1]]
    true_errs[r] <- errori_boot[[2]]
    # errore quadratico per la r-esima ripetizione
    differenza_boot_cubo <- (errori_boot[[1]]-errori_boot[[2]])^2
    differenze_boot_MSE <- c(differenze_boot_MSE, differenza_boot_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_boot <- (errori_boot[[1]]-errori_boot[[2]])
    differenze_boot_BIAS <- c(differenze_boot_BIAS, differenza_boot)
  }
  # MSE
  MSE_boot <- sum(differenze_boot_MSE)/R
  # BIAS
  BIAS_boot <- sum(differenze_boot_BIAS)/R
  # restituzione risultati
  result_list <- list(MSE = MSE_boot, Bias = BIAS_boot, errori=boot_errs, true_errors= mean(true_errs))
  return(result_list)
}


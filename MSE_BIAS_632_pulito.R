
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

#### CALCOLO ERRORE BOOTSTRAP .632 ####
errorest_632 <- function(x, y, z, w, train, classify, num_bootstraps = 100,
                         seed = NULL , ...) {
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
  # calcolo errore su Test (w+z)
  train_test <- train(x, y, ...)
  classifications_test <- classify(object = train_test, newdata = z)
  true_err <- mean(w != classifications_test)
  # restituzione risultati
  result_list <- list(boot_err_632, true_err)
  return(result_list)
}

#### CALCOLO MSE E BIAS .632 ####
MSE_BIAS_632 <- function(dataset, classe, train, classify, num_bootstraps = 100,
                         seed = NULL , R = NULL , ...) {
  differenze_632_MSE <- numeric(0)
  differenze_632_BIAS <- numeric(0)
  boot632_errs <- numeric(R)
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
    # errori .632
    errori_632 <- errorest_632(x, y, z, w, train, classify, num_bootstraps = num_bootstraps,
                               seed = seeds[r], ...)
    boot632_errs[r] <- errori_632[[1]]
    true_errs[r] <- errori_632[[2]]
    # errore quadratico per la r-esima ripetizione
    differenza_632_cubo <- (errori_632[[1]]-errori_632[[2]])^2
    differenze_632_MSE <- c(differenze_632_MSE, differenza_632_cubo)
    # errore semplice per la r-esima ripetizione
    differenza_632 <- (errori_632[[1]]-errori_632[[2]])
    differenze_632_BIAS <- c(differenze_632_BIAS, differenza_632)
  }
  # MSE
  MSE_632 <- sum(differenze_632_MSE)/R
  # BIAS
  BIAS_632 <- sum(differenze_632_BIAS)/R
  # restituzione risultati
  result_list <- list(MSE = MSE_632, Bias = BIAS_632, errori = boot632_errs, true_errors = mean(true_errs))
  return(result_list)
}

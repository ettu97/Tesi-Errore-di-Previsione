table(data$class)

skim(data)

str(data)



MSE_BIAS_BCV(data , classe=451, R=10, seed=123, train=train_7nn, classify=classify_knn)


tabella_alz <- tabella_confronti(dataset = data, classe = 451, R = 1, seed = 123)

classe=12
dataset <- data
dataset <- dataset %>% select(-ID)

str(dataset)

data <- data.frame(
  MSE1_5nn = c(boot_5nn$MSE , b632_5nn$MSE, b632_plus_5nn$MSE, cv_5nn$MSE, loocv_5nn$MSE, bcv_5nn$MSE), 
  Bias1_5nn = c(boot_5nn$Bias , b632_5nn$Bias, b632_plus_5nn$Bias, cv_5nn$Bias, loocv_5nn$Bias, bcv_5nn$Bias),
  MSE2_7nn = c(boot_7nn$MSE , b632_7nn$MSE, b632_plus_7nn$MSE, cv_7nn$MSE, loocv_7nn$MSE, bcv_7nn$MSE), 
  Bias2_7nn = c(boot_7nn$Bias , b632_7nn$Bias, b632_plus_7nn$Bias, cv_7nn$Bias, loocv_7nn$Bias, bcv_7nn$Bias),
  MSE3_tree = c(boot_tree$MSE , b632_tree$MSE, b632_plus_tree$MSE, cv_tree$MSE, loocv_tree$MSE, bcv_tree$MSE), 
  Bias3_tree = c(boot_tree$Bias , b632_tree$Bias, b632_plus_tree$Bias, cv_tree$Bias, loocv_tree$Bias, bcv_tree$Bias)
)

data <- data %>%
  mutate(across(starts_with("MSE"), sqrt))
rownames(data) <- c("boot", ".632", ".632+", "10-fold CV", "LOOCV", "BCV")
data <- cbind(Row = rownames(data), data)

# colonne
colnames(data) <- c("Stimatore", rep(c("RMSE", "Bias"), 3))

# tabella
tabella_hearts <- kable(data, "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  add_header_above(c(" " = 1, "5-NN" = 2, "7-NN" = 2, "TREE" = 2))  %>%
  kable_styling(bootstrap_options = c("hover", "bordered", "striped", "responsive"))
tabella_hearts


R=50





################### BOXPLOTS TREE #################

errors_df <- data.frame(
  boot_tree$errori,
  b632_tree$errori, 
  b632_plus_tree$errori,
  cv_tree$errori,
  loocv_tree$errori,
  bcv_tree$errori
)

# 
errors_long <- pivot_longer(errors_df, 
                            cols = everything(), 
                            names_to = "Method", 
                            values_to = "ErrorRate")

# 
media_true <- (mean(boot_tree$true_errors)+
                 mean(b632_tree$true_errors)+ 
                 mean(b632_plus_tree$true_errors)+
                 mean(cv_tree$true_errors)+
                 mean(loocv_tree$true_errors)+
                 mean(bcv_tree$true_errors))/6






boxplot_tree <- ggplot(errors_long, aes(x = Method, y = ErrorRate)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = media_true, color = "red", linetype = "dashed", size = 1) +
  scale_x_discrete(labels = c("boot_tree.errori" = "boot",
                              "b632_tree.errori" = "boot .632",
                              "b632_plus_tree.errori" = "boot .632+",
                              "cv_tree.errori" = "10-fold CV",
                              "loocv_tree.errori" = "LOOCV",
                              "bcv_tree.errori" = "BCV")) +
  labs(title = "Boxplot of Different Error Estimations for TREE",
       y = "Error Rate",
       x = "Method") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
boxplot_tree



prova <- normalize(heart_statlog_cleveland_hungary_final) %>% mutate(target=as.factor(target))
str(prova)

table(prova$target)

################### BOXPLOTS 7NN #################

errors_df <- data.frame(
  boot_7nn$errori,
  b632_7nn$errori, 
  b632_plus_7nn$errori,
  cv_7nn$errori,
  loocv_7nn$errori,
  bcv_7nn$errori
)

media_true <- (mean(boot_7nn$true_errors)+
                 mean(b632_7nn$true_errors)+ 
                 mean(b632_plus_7nn$true_errors)+
                 mean(cv_7nn$true_errors)+
                 mean(loocv_7nn$true_errors)+
                 mean(bcv_7nn$true_errors))/6


# 
errors_long <- pivot_longer(errors_df, 
                            cols = everything(), 
                            names_to = "Method", 
                            values_to = "ErrorRate")

# 
boxplot_7nn <- ggplot(errors_long, aes(x = Method, y = ErrorRate)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = media_true, color = "red", linetype = "dashed", size = 1) +
  scale_x_discrete(labels = c("boot_7nn.errori" = "boot",
                              "b632_7nn.errori" = "boot .632",
                              "b632_plus_7nn.errori" = "boot .632+",
                              "cv_7nn.errori" = "10-fold CV",
                              "loocv_7nn.errori" = "LOOCV",
                              "bcv_7nn.errori" = "BCV")) +
  labs(title = "Boxplot of Different Error Estimations for 7NN",
       y = "Error Rate",
       x = "Method") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
boxplot_7nn

################### BOXPLOTS 5NN #################

errors_df <- data.frame(
  boot_5nn$errori,
  b632_5nn$errori, 
  b632_plus_5nn$errori,
  cv_5nn$errori,
  loocv_5nn$errori,
  bcv_5nn$errori
)

media_true <- (mean(boot_5nn$true_errors)+
                 mean(b632_5nn$true_errors)+ 
                 mean(b632_plus_5nn$true_errors)+
                 mean(cv_5nn$true_errors)+
                 mean(loocv_5nn$true_errors)+
                 mean(bcv_5nn$true_errors))/6


# 
errors_long <- pivot_longer(errors_df, 
                            cols = everything(), 
                            names_to = "Method", 
                            values_to = "ErrorRate")

# 

boxplot_5nn <- ggplot(errors_long, aes(x = Method, y = ErrorRate)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = media_true, color = "red", linetype = "dashed", size = 1) +
  scale_x_discrete(labels = c("boot_5nn.errori" = "boot",
                              "b632_5nn.errori" = "boot .632",
                              "b632_plus_5nn.errori" = "boot .632+",
                              "cv_5nn.errori" = "10-fold CV",
                              "loocv_5nn.errori" = "LOOCV",
                              "bcv_5nn.errori" = "BCV")) +
  labs(title = "Boxplot of Different Error Estimations for 5NN",
       y = "Error Rate",
       x = "Method") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


boxplot_5nn

################## PLOT BOOT ###########


stimati <- as.data.frame(boot_7nn$errori)
veri <- as.data.frame(boot_7nn$true_errors)

stimati$Index <- rownames(stimati)  # Create an index column
stimati$Index <- as.numeric(stimati$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri,stimati)


# Crea il plot
p <- ggplot() +
  geom_line(data = veriVSstimati, aes(x = Index, y = boot_7nn$true_errors, color = "True Errors")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = boot_7nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = boot_7nn$errori, color = "Boot Estimates")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = boot_7nn$errori, color = "Boot Estimates")) +
  
  geom_hline(aes(yintercept = mean(boot_7nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(boot_7nn$errori), color = "Average Boot Estimate"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs Boot Estimates con 7NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", "Boot Estimates" = "red",
                                "Average True Error" = "blue4", "Average Boot Estimate" = "red4")) +
  theme_minimal()

# Stampa il plot
p
#ggplotly(p)



################## PLOT .632 ###########

stimati632 <- as.data.frame(b632_7nn$errori)
veri632 <- as.data.frame(b632_7nn$true_errors)

stimati632$Index <- rownames(stimati632)  # Create an index column
stimati632$Index <- as.numeric(stimati632$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati632 <- cbind(veri632,stimati632)


# Crea il plot
q <- ggplot() +
  geom_line(data = veriVSstimati632, aes(x = Index, y = b632_7nn$true_errors, color = "True Errors")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = b632_7nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati632, aes(x = Index, y = b632_7nn$errori, color = ".632 Estimates")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = b632_7nn$errori, color = ".632 Estimates")) +
  
  geom_hline(aes(yintercept = mean(b632_7nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(b632_7nn$errori), color = "Average .632 Estimate"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs .632 Estimates con 7NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", ".632 Estimates" = "red",
                                "Average True Error" = "blue4", "Average .632 Estimate" = "red4")) +
  theme_minimal()
q
# Stampa il plot

#ggplotly(q)

################## PLOT .632 PLUS ###########

stimati <- as.data.frame(b632_plus_7nn$errori)
veri <- as.data.frame(b632_plus_7nn$true_errors)

stimati$Index <- rownames(stimati)  # Create an index column
stimati$Index <- as.numeric(stimati$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri,stimati)


# Crea il plot
r <- ggplot() +
  geom_line(data = veriVSstimati, aes(x = Index, y = b632_plus_7nn$true_errors, color = "True Errors")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = b632_plus_7nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = b632_plus_7nn$errori, color = ".632+ Estimates")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = b632_plus_7nn$errori, color = ".632+ Estimates")) +
  
  geom_hline(aes(yintercept = mean(b632_plus_7nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(b632_plus_7nn$errori), color = "Average .632+ Estimate"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs .632+ Estimates con 7NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", ".632+ Estimates" = "red",
                                "Average True Error" = "blue4", "Average .632+ Estimate" = "red4")) +
  theme_minimal()
r
# Stampa il plot

#ggplotly(q)



################## PLOT BCV ###########

stimati <- as.data.frame(bcv_7nn$errori)
veri <- as.data.frame(bcv_7nn$true_errors)

stimati$Index <- rownames(stimati)  # Create an index column
stimati$Index <- as.numeric(stimati$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri,stimati)


# Crea il plot
s <- ggplot() +
  geom_line(data = veriVSstimati, aes(x = Index, y = bcv_7nn$true_errors, color = "True Errors")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = bcv_7nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = bcv_7nn$errori, color = "BCV Estimates")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = bcv_7nn$errori, color = "BCV Estimates")) +
  
  geom_hline(aes(yintercept = mean(bcv_7nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(bcv_7nn$errori), color = "Average BCV Estimate"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs BCV Estimates con 7NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", "BCV Estimates" = "red",
                                "Average True Error" = "blue4", "Average BCV Estimate" = "red4")) +
  theme_minimal()
s
# Stampa il plot

#ggplotly(q)

################## PLOT LOOCV ###########

stimatiloocv <- as.data.frame(loocv_7nn$errori)
veriloocv <- as.data.frame(loocv_7nn$true_errors)

stimatiloocv$Index <- rownames(stimatiloocv)  # Create an index column
stimatiloocv$Index <- as.numeric(stimatiloocv$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimatiloocv <- cbind(veriloocv,stimatiloocv)


# Crea il plot
t <- ggplot() +
  geom_line(data = veriVSstimatiloocv, aes(x = Index, y = loocv_7nn$true_errors, color = "True Errors")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = loocv_7nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimatiloocv, aes(x = Index, y = loocv_7nn$errori, color = "LOOCV Estimates")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = loocv_7nn$errori, color = "LOOCV Estimates")) +
  
  geom_hline(aes(yintercept = mean(loocv_7nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(loocv_7nn$errori), color = "Average LOOCV Estimate"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs LOOCV Estimates con 7NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", "LOOCV Estimates" = "red",
                                "Average True Error" = "blue4", "Average LOOCV Estimate" = "red4")) +
  theme_minimal()
t
# Stampa il plot

#ggplotly(q)

################## PLOT 10-fold CV ###########

stimati <- as.data.frame(cv_7nn$errori)
veri <- as.data.frame(cv_7nn$true_errors)

stimati$Index <- rownames(stimati)  # Create an index column
stimati$Index <- as.numeric(stimati$Index)  # Convert index to numeric if necessary

#veri$Index <- rownames(veri)  # Create an index column
#veri$Index <- as.numeric(veri$Index)  # Convert index to numeric if necessary

veriVSstimati <- cbind(veri,stimati)


# Crea il plot
u <- ggplot() +
  geom_line(data = veriVSstimati, aes(x = Index, y = cv_7nn$true_errors, color = "True Errors")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = cv_7nn$true_errors, color = "True Errors")) +
  geom_line(data = veriVSstimati, aes(x = Index, y = cv_7nn$errori, color = "10-fold CV Estimates")) +
  #geom_point(data = veriVSstimati, aes(x = Index, y = cv_7nn$errori, color = "10-fold CV Estimates")) +
  
  geom_hline(aes(yintercept = mean(cv_7nn$true_errors), color = "Average True Error"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_hline(aes(yintercept = mean(cv_7nn$errori), color = "Average 10-fold CV Estimate"), 
             linetype = "dashed", size = 1, show.legend = TRUE) +
  
  labs(title = "Plot di True Errors vs 10-fold CV Estimates con 7NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("True Errors" = "blue", "10-fold CV Estimates" = "red",
                                "Average True Error" = "blue4", "Average 10-fold CV Estimate" = "red4")) +
  theme_minimal()
u
# Stampa il plot

#ggplotly(q)


##############  .632 vs loocv #########

veriVSstimati <- cbind(stimati632[,c(1)],stimatiloocv)
colnames(veriVSstimati) <- c("boot632", "loocv","Index")


# Crea il plot
loocvVS.632 <- ggplot() +
  geom_line(data = veriVSstimati, aes(x = Index, y = loocv, color = "LOOCV Estimates")) +

  geom_line(data = veriVSstimati, aes(x = Index, y = boot632, color = "Boot .632 Estimates")) +
  
  #geom_hline(aes(yintercept = mean(loocv_7nn$true_errors), color = "Average True Error"), 
  #           linetype = "dashed", size = 1, show.legend = TRUE) +
  #geom_hline(aes(yintercept = mean(loocv_7nn$errori), color = "Average LOOCV Estimate"), 
  #           linetype = "dashed", size = 1, show.legend = TRUE) +
  
  labs(title = "Plot di .632 Estimates vs LOOCV Estimates con 7NN", x = "Iterazione", y = "Valore", color = "Legenda") +
  
  scale_color_manual(values = c("LOOCV Estimates" = "blue", "Boot .632 Estimates" = "red")) +
  theme_minimal()

loocvVS.632

#########################








































































































































































































































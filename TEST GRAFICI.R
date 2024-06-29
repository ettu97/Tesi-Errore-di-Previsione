table(data$class)

skim(data)

str(data)



MSE_BIAS_BCV(data , classe=451, R=10, seed=123, train=train_7nn, classify=classify_knn)


tabella_alz <- tabella_confronti(dataset = data, classe = 451, R = 1, seed = 123)

classe=451
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
kable(data, "html", row.names = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  add_header_above(c(" " = 1, "5-NN" = 2, "7-NN" = 2, "TREE" = 2))  %>%
  kable_styling(bootstrap_options = c("hover", "bordered", "striped", "responsive"))

R=50





################### BOXPLOTS #################

errors_df <- data.frame(
  boot_7nn$errori,
  b632_7nn$errori, 
  b632_plus_7nn$errori,
  cv_7nn$errori,
  loocv_7nn$errori,
  bcv_7nn$errori
)

# Transform data to long format
errors_long <- pivot_longer(errors_df, 
                            cols = everything(), 
                            names_to = "Method", 
                            values_to = "ErrorRate")

# Create a boxplot with ggplot2
ggplot(errors_long, aes(x = Method, y = ErrorRate)) +
  geom_boxplot(fill = "lightblue") +
  geom_hline(yintercept = boot_7nn$true_errors, color = "red", linetype = "dashed", size = 5) +
  geom_hline(yintercept = b632_7nn$true_errors, color = "green", linetype = "dashed", size = 4) +
  geom_hline(yintercept = b632_plus_7nn$true_errors, color = "purple", linetype = "dashed", size = 3) +
  geom_hline(yintercept = cv_7nn$true_errors, color = "yellow", linetype = "dashed", size = 2) +
  geom_hline(yintercept = loocv_7nn$true_errors, color = "cyan", linetype = "dashed", size = 1) +
  geom_hline(yintercept = bcv_7nn$true_errors, color = "brown", linetype = "dashed", size = 0.5) +
  labs(title = "Boxplot of Different Error Estimations for 7NN",
       y = "Error Rate",
       x = "Method") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

boot_5nn$true_errors
 


dataset <- dataset %>% select(-ID)



heatmaply_cor(cor, limits = c(-1, 1), colors = cool_warm)

heatmaply_cor(
  cor(dataset[,-451]),
  xlab = "Features",
  ylab = "Features",
  k_col = 2,
  k_row = 2
)






























































































































































































































































































































library(ggplot2)
library(corrplot)
library(class)
library(pROC)
library(RColorBrewer)



# Data Import


Data = read.csv2(" ./Dataset/bank_accounts_train.csv", 
                 header = T, 
                 sep = ",", 
                 colClasses = "character")


# Data Preprocessing


Data$CLIENTNUM = NULL 
str(Data)


variables = colnames(Data) # With this line we have a vector of our variables
categorical_variables <- c("Gender", "Education_Level", "Marital_Status", "Card_Category") # We select our categorical variables (only 4 of them)
target_variable = "Closed_Account"
numerical_variables <- setdiff(variables, c(categorical_variables, target_variable)) 
for (var in numerical_variables) {
  Data[[var]] = as.numeric(Data[[var]])
}

for (var in categorical_variables) {
  Data[[var]] = as.factor(Data[[var]])
}

Data$Closed_Account = as.factor(Data$Closed_Account)

# EDA

## Target Variable Distribution


x = table(Data$Closed_Account)
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
{pie(x = table(Data$Closed_Account), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90, 
     col = c("green",
             "red"),
     cex = 1.6)
  mtext("Closed Account", side = 3, cex = 2)
  legend("topleft", 
         pch = 15, 
         col = c("green", "red"),
         c("Not churn", "Churn"), cex = 1.2,
         bty = "n")}



## Categorical Variables exploration

### Gender


x = table(Data$Gender)
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
{pie(x = table(Data$Gender), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90, 
     col = c(rgb(1,0,0, .5),
             rgb(0,0,1,0.5)),
     cex = 2)
  mtext("Gender", side = 3, cex = 2)
  legend("topright", 
         pch = 15, 
         col = c(rgb(1,0,0, .5),
                 rgb(0,0,1,0.5)),
         c("Female", "Male"), cex = 1.7,
         bty = "n")}

##Gender-Closed_Account
#Adjusting the dimensions of the legend
smaller_text_theme <- theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12),
        strip.text.x = element_text(size = 10))

ggplot(Data, aes(x = Gender, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
labs(title = "Proportion of Closed Account By Gender", x = "Gender", y = "Proportion") +
  smaller_text_theme


### Marital Status


#MARITAL STATUS
x <- table(addNA(Data$Marital_Status))
r = round(x/nrow(Data)*100, 2)
s = paste( r, "%", sep = "")
{pie(x = table(Data$Marital_Status), 
     labels = s,
     edges = 10000, 
     radius = 1,
     init.angle = 90, 
     col = c(rgb(1,0,0,0.5),
             rgb(0,0,1,0.5),
             rgb(0,0,0.5,1)
     ),
     cex = 1)
  mtext("Marital Status", side = 3, cex = 1.5, line = 1)
  legend("topright", 
         pch = 15, 
         col = c(rgb(1,0,0, .5),
                 rgb(0,0,1,0.5),
                 rgb(0,0, .5,1)
         ),
         c("Married", "Single", "Divorced"), cex = 1,
         bty = "n")}

##Marital_Status-Closed_Account

ggplot(Data, aes(x = Marital_Status, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
  labs(title = "Proportion of Closed Accounts by Marital_Status", x = "Income Bin", y = "Proportion") +
  smaller_text_theme


### Education Level

#EDUCATION LEVEL
education_table <- table(Data$Education_Level)
r <- round(education_table / nrow(Data) * 100, 2)
s <- paste(r, "%", sep = "")

pie(x = education_table, 
    labels = s,
    edges = 10000, 
    radius = 1,
    init.angle = 90, 
    col = c(rgb(1,0,0,0.5),
            rgb(0,0,1,0.5),
            rgb(0,0,0.5,1),
            rgb(0.5,0.5,0,1),
            rgb(0.3,0,0.5,0.8),
            rgb(1,0.8,0,0.5)),
    cex = 1)
mtext("Education Level", side = 3, cex = 1.5, line = 1) # side=3 is the top
legend("topleft", 
       pch = 15, 
       col = c(rgb(1,0,0,0.5),
               rgb(0,0,1,0.5),
               rgb(0,0,0.5,1),
               rgb(0.5,0.5,0,1),
               rgb(0.3,0,0.5,0.8),
               rgb(1,0.8,0,0.5)),
       legend = levels(Data$Education_Level), cex = 1,
       bty = "n")

##Education_Level-Closed_Account

ggplot(Data, aes(x = Education_Level, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
  labs(title = "Proportion of Closed Accounts by Education_Level", x = "Education_Level", y = "Proportion") +
  smaller_text_theme


### Card Category

# Card Category
card_table <- table(Data$Card_Category)
r <- round(card_table / nrow(Data) * 100, 2)
s <- paste(r, "%", sep = "")
s2 <- paste(levels(Data$Card_Category), s)

pie(x = card_table,
    edges = 10000, 
    radius = 1,
    init.angle = 90, 
    col = c(rgb(1,0,0,0.5),
            rgb(0,0,1,0.5),
            rgb(0,0,0.5,1),
            rgb(0.5,0.5,0,1)
    ),
    cex = 1)
mtext("Card Category", side = 3, cex = 1.5, line = 1) # side=3 is the top
legend( x= -2.4, y = 1,
        pch = 15, 
        col = c(rgb(1,0,0,0.5),
                rgb(0,0,1,0.5),
                rgb(0,0,0.5,1),
                rgb(0.5,0.5,0,1)
        ),
        legend = s2, cex = 1,
        bty = "n")

##Card_Category-Customer_Age
ggplot(Data, aes(x = Card_Category, fill = as.factor(Closed_Account))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "#FFFF99"), labels = c("Account Open", "Account Closed"), name = "") +
  labs(title = "Proportion of Closed Accounts by Card_Category", x = "Card_Category", y = "Proportion") +
  smaller_text_theme



### Graphical Exploration of Numerical Variables

Numerical_Data = Data[numerical_variables]
means_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = mean)
median_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = median)
sd_vec = apply(X = Numerical_Data, MARGIN = 2, FUN = sd)

par(mfrow = c(3,3), mar = c(2,4,4,1))

for(i in 1:length(numerical_variables)){
  hist(Numerical_Data[,i], freq = F, main = names(Numerical_Data)[i],
       col = rgb(.7,.7,.7), border = "white", xlab = "")
  abline(v = means_vec[i], lwd = 2)
  abline(v = median_vec[i], lwd = 2, col = rgb(.7,0,0))
  legend("top", c("Mean", "Median"), lwd = 2, col = c(1, rgb(.7,0,0)),cex = .8, bty = "n")
}


### Correlation Matrix

cor_matrix <- cor(Data[numerical_variables], use="complete.obs")
# Increase the size of the margins 
par(mar = c(5, 8, 4, 2) + 0.1) 

corrplot(cor_matrix, method = "color", tl.srt = 45, tl.col = "black") 

par(mar = c(5, 4, 4, 2) + 0.1) # Default size

## Correlation with target variable


target_variable_numeric = as.numeric(Data$Closed_Account)

  correlations <- sapply(Data[numerical_variables], function(x) cor(x, target_variable_numeric, use = "complete.obs"))

# Omit the target variable from the plot if it's included in the Data frame
correlations <- abs(correlations[names(correlations) != "target_var"])

# Increase the size of the margins 
par(mar = c(5, 8, 4, 2) + 0.1)  

# Create the barplot with larger font size for names
barplot(correlations, main="Correlation with Target Variable",
        horiz=TRUE, cex.names=0.7, las=2, col = "deepskyblue")

par(mar = c(5, 4, 4, 2) + 0.1) # Default size


## Dealing with missing values



#Rename missing values with NA notation
Data[Data == 'Unknown'] <- NA



# Now we try to understand where the missing values are

# Sum the TRUE values by row to see how many NAs each row contains
na_counts_per_row <- rowSums(is.na(Data))

# Count how many rows have at least one NA
rows_with_na <- sum(na_counts_per_row > 0)

# Print the result
cat("Rows with NA before preprocessing:", rows_with_na, "\n")


#Basic function to compute mode
getSimpleMode <- function(x) {
  tbl <- table(x)
  mode_value <- names(tbl[tbl == max(tbl)])[1]
  return(mode_value)
}


for (var in categorical_variables) {
  if (any(is.na(Data[[var]]))) {
    Data[[var]][is.na(Data[[var]])] <- getSimpleMode(Data[[var]])
  }
}


# Sum the TRUE values by row to see how many NAs each row contains
na_counts_per_row <- rowSums(is.na(Data))

# Count how many rows have at least one NA
rows_with_na <- sum(na_counts_per_row > 0)

# Print the result
cat("Rows with NA after preprocessing:", rows_with_na, "\n")


# 3 Logistic Regression Model to estimate effects of Income and Gender on Closed_Account

# Fit logistic regression model
set.seed(1)
gender_income_model <- glm(Closed_Account ~ Income * Gender, 
                           family = binomial(link = "logit"), 
                           data = Data)

# View model summary
summary(gender_income_model)


# Compute and store predicted probabilities
predicted_probabilities_logistic <- predict(gender_income_model, newdata = Data, type="response")

Data$predicted_probabilities <- predict(gender_income_model, newdata = Data, type="response")


#Plotting computed probabilities
ggplot(Data, aes(x = Income, y = predicted_probabilities, color = Gender)) + 
  geom_line() + 
  labs(title = "Probability of Account Closure by Income and Gender", y = "Probability of Closure", x = "Income") +
  scale_color_manual(values = c("red", "blue"))




Data$predicted_probabilities = NULL # We drop the column of predicted probabilities since it is now useless



# 4 KNN

# Seed setting
set.seed(10)

# Split Dataset
id_train <- sample(1:nrow(Data), size = 0.75*nrow(Data), replace = F)
train_data <- Data[id_train,]
val_data <- Data[-id_train,]

# Response variable distribution in the original data
cat("Distribution of the target variable in the original set:\n")
cat("Counts:")
print(table(Data$Closed_Account))
cat("Proportions:")
print(prop.table(table(Data$Closed_Account)))
cat("\n")

# Response variable distribution in the train test
cat("Distribution of the target variable in the train set:\n")
cat("Counts:")
print(table(train_data$Closed_Account))
cat("Proportions:")
print(prop.table(table(train_data$Closed_Account)))
cat("\n")

# Response variable distribution in the validation set
cat("Distribution of the target variable in the validation set:\n")
cat("Counts:")
print(table(val_data$Closed_Account))
cat("Proportions:")
print(prop.table(table(val_data$Closed_Account)))


calculate_sensitivity <- function(true_values, predictions) {
  true_positives <- sum(true_values == 1 & predictions == 1)
  actual_positives <- sum(true_values == 1)
  return(true_positives / actual_positives)
}

# Set a range for k
k_values <- 1:25
accuracy_scores <- numeric(length(k_values))
sensitivity_scores <- numeric(length(k_values))

# Loop over k values
for (k in k_values) {
  set.seed(100) # for reproducibility
  knn_pred <- knn(train = train_data[, c("Total_Trans_Amt", "Total_Trans_Ct")],
                  test = val_data[, c("Total_Trans_Amt", "Total_Trans_Ct")],
                  cl = train_data$Closed_Account,
                  k = k)
  
  # Calculate accuracy
  accuracy_scores[k] <- sum(val_data$Closed_Account == knn_pred) / length(knn_pred)
  # Calculate sensitivity
  sensitivity_scores[k] <- calculate_sensitivity(val_data$Closed_Account, knn_pred)
}


# Plot the accuracy scores as a function of k
plot(k_values, accuracy_scores, type = "b", 
     xlab = "Number of Neighbors (k)", ylab = "Accuracy",
     main = "k-NN Model Accuracy by Number of Neighbors")

# Plot the accuracy scores as a function of k
plot(k_values, sensitivity_scores, type = "b", 
     xlab = "Number of Neighbors (k)", ylab = "Sensitivity",
     main = "k-NN Model Sensitivity by Number of Neighbors")


# For accuracy
best_accuracy_k <- which.max(accuracy_scores)
cat("The best number of neighbors for accuracy is:", best_accuracy_k, 
    "with an accuracy of:", max(accuracy_scores), "\n")

# Exclude the best to find the second best
accuracy_scores[best_accuracy_k] <- NA
second_best_accuracy_k <- which.max(accuracy_scores)
cat("The second-best number of neighbors for accuracy is:", second_best_accuracy_k, 
    "with an accuracy of:", max(accuracy_scores, na.rm = TRUE), "\n")

# For sensitivity
best_sensitivity_k <- which.max(sensitivity_scores)
cat("The best number of neighbors for sensitivity is:", best_sensitivity_k, 
    "with a sensitivity of:", max(sensitivity_scores), "\n")

# Exclude the best to find the second best
sensitivity_scores[best_sensitivity_k] <- NA
second_best_sensitivity_k <- which.max(sensitivity_scores)
cat("The second-best number of neighbors for sensitivity is:", second_best_sensitivity_k, 
    "with a sensitivity of:", max(sensitivity_scores, na.rm = TRUE), "\n")

# 5 Best Model Selection

logit_fit_baseline <- glm(Closed_Account ~ .,
                          family = "binomial",
                          data = train_data)


logit_fit_0 <- glm(Closed_Account ~ 1,
                   family = "binomial",
                   data = train_data)


logit_fit0 <- glm(Closed_Account ~ 1,
                  family = "binomial",
                  data = train_data)
anova(logit_fit_0, logit_fit_baseline, test = "Chisq") 




## Variable Selection with AIC and BIC



### Stepwise variable selection (based on AIC) 

# Forward
logit_fit_aic1 <- step(glm(Closed_Account ~ 1,
                           family = "binomial",
                           data = train_data),
                       scope = formula(logit_fit_baseline),
                       direction = "forward")

# Backward
logit_fit_aic2 <- step(logit_fit_baseline,
                       direction = "backward") 


# Both directions
logit_fit_aic3 <- step(logit_fit_baseline,
                       direction = "both")


print(length(coefficients(logit_fit_aic1)))
print(length(coefficients(logit_fit_aic2)))
print(length(coefficients(logit_fit_aic3)))


### Stepwise variable selection (based on BIC) 


# Forward
logit_fit_bic1 <- step(glm(Closed_Account ~ 1,
                           family = "binomial",
                           data = train_data),
                       scope = formula(logit_fit_baseline),
                       direction = "forward",
                       k = log(nrow(train_data)))


# Backward
logit_fit_bic2 <- step(logit_fit_baseline,
                       direction = "backward",
                       k = log(nrow(train_data)))  



# Both directions
logit_fit_bic3 <- step(logit_fit_baseline,
                       direction = "both",
                       k = log(nrow(train_data)))




print(length(coefficients(logit_fit_bic1)))
print(length(coefficients(logit_fit_bic2)))
print(length(coefficients(logit_fit_bic3)))


## PCA 


# Selecting Numerical DF
train_data_Numerical = train_data[numerical_variables] 

cov(train_data_Numerical)

# Scaling Data
train_data_Numerical_Scaled= scale(train_data_Numerical)

cov(train_data_Numerical_Scaled)


pca <- princomp(train_data_Numerical_Scaled, cor = T, scale = F) #already scaled

pca$loadings

# Calculating the variance (in percentage) explained by each PCA component
pca_var <- pca$sdev^2
pca_var_percent <- pca_var / sum(pca_var)


bar.comp = barplot(pca_var_percent,
                   las = 2,
                   col = rev(brewer.pal(9, "Blues")), 
                   border = F,
                   ylim = c(0, max(pca_var_percent)*1.5),
                   ylab = "Explained variance")

# choose components according to "elbow rule"

lines(x = bar.comp, pca_var_percent, type = "b", pch = 16, cex = 1.5, lwd = 2, col = 2)
grid()


# Calculating the cumulative variance explained
cum_pca_var_percent <- cumsum(pca_var_percent)*100

# Plotting the cumulative distribution

bar.comp = barplot(cum_pca_var_percent,
                   las = 2,
                   col = rev(brewer.pal(9, "Blues")), 
                   border = F,
                   ylim = c(0, 105),
                   ylab = "Explained variance")

lines(bar.comp, cum_pca_var_percent, type = "b", pch = 16, cex = 1.5, lwd = 2, col = 2)
grid()


# Validation data is scaled as the train data
val_data_Numerical = val_data[numerical_variables] 
val_data_Numerical_Scaled= scale(val_data_Numerical)

# Predicting scores for the validation set using the PCA model
pca_data <- predict(pca, train_data_Numerical_Scaled)

# Selecting the first 11 principal components
pca_data <- pca_data[, 1:11]


logit_pca = glm(Closed_Account ~ ., 
                data = data.frame(pca_data, 
                                  Closed_Account = train_data$Closed_Account), 
                family = "binomial")



## Best model evaluation

### AUC on the training set


roc_aic1 <- pROC::roc(train_data$Closed_Account,
                      logit_fit_aic1$fitted.values,
                      plot = TRUE,
                      col = "red",
                      lwd = 3,
                      auc.polygon = T,
                      auc.polygon.col = "deepskyblue",
                      print.auc = T)

roc_bic1 <- pROC::roc(train_data$Closed_Account,
                      logit_fit_bic1$fitted.values,
                      plot = TRUE,
                      col = "red",
                      lwd = 3,
                      auc.polygon = T,
                      auc.polygon.col = "deepskyblue",
                      print.auc = T)

roc_pca <- pROC::roc(train_data$Closed_Account,
                     logit_pca$fitted.values,
                     plot = TRUE,
                     col = "red",
                     lwd = 3,
                     auc.polygon = T,
                     auc.polygon.col = "deepskyblue",
                     print.auc = T)



roc_pca <- pROC::roc(train_data$Closed_Account,
                     logit_pca$fitted.values,
                     plot = TRUE,
                     col = "red",
                     lwd = 3,
                     auc.polygon = T,
                     auc.polygon.col = "deepskyblue",
                     print.auc = T)



###  Models testing on the validation set

## Validation set ##

#threshold
tt=0.5

# Predictions for the observations in the validation set
prob_out_aic1 <- predict(logit_fit_aic1,
                         newdata = val_data[,-1],
                         type = "response")

pred_out_aic1 <- as.factor(ifelse(prob_out_aic1 > tt, "yes", "no"))


prob_out_bic1 <- predict(logit_fit_bic1,
                         newdata = val_data[,-1],
                         type = "response")
pred_out_bic1 <- as.factor(ifelse(prob_out_bic1 > tt, "yes", "no"))


# ROC curves
roc_out_aic1 <- pROC::roc(val_data$Closed_Account,
                          prob_out_aic1,
                          plot = TRUE,
                          col = "lightgreen",
                          lwd = 3,
                          auc.polygon = T,
                          auc.polygon.col = "#FFFF99",
                          print.auc = T)

roc_out_bic1 <- pROC::roc(val_data$Closed_Account,
                          prob_out_bic1,
                          plot = TRUE,
                          col = "lightgreen",
                          lwd = 3,
                          auc.polygon = T,
                          auc.polygon.col = "#FFFF99",
                          print.auc = T)

roc_out_pca <- pROC::roc(val_data$Closed_Account,
                         prob_out_bic1,
                         plot = TRUE,
                         col = "lightgreen",
                         lwd = 3,
                         auc.polygon = T,
                         auc.polygon.col = "#FFFF99",
                         print.auc = T)




## Now let's take the best model and fit in the test dataset...

TestData = read.csv2("./Dataset/bank_accounts_test.csv", 
                     header = T, 
                     sep = ",", 
                     colClasses = "character")

#Encode NaN values
#Rename missing values with NA notation
TestData[TestData == 'Unknown'] <- NA

for (var in categorical_variables) {
  if (any(is.na(TestData[[var]]))) {
    TestData[[var]][is.na(TestData[[var]])] <- getSimpleMode(TestData[[var]])
  }
}

##substitute the type of the variables from "ch" to "num" and "factor"
for (var in numerical_variables) {
  TestData[[var]] = as.numeric(TestData[[var]])
}

for (var in categorical_variables) {
  TestData[[var]] = as.factor(TestData[[var]])
}

# Predictions for the observations in the Test set
predicted_prob <- predict(logit_fit_aic2,
                          newdata = TestData,
                          type = "response")

head(predicted_prob)

##Write the .csv file that contains the predicted probabilities.
write.csv(predicted_prob, "my_prob.csv", row.names = F)

# 6 TRESHOLD CHOICE

# Thresholds choices
thresholds <- c(0.5, 0.2, 0.8, 0.28) 

# Function to calculate metrics and financial outcome
calculate_metrics <- function(tt) {
  prob_out_best <- predict(logit_fit_bic1, newdata = val_data, type = "response")
  pred_out_best_binary <- ifelse(prob_out_best > tt, 1, 0)
  conf_matrix <- table(Predicted = pred_out_best_binary, 
                       Actual = val_data$Closed_Account)
  
  # Extracting elements from the confusion matrix
  TP <- conf_matrix["1", "1"]
  TN <- conf_matrix["0", "0"]
  FP <- conf_matrix["1", "0"]
  FN <- conf_matrix["0", "1"]
  
  # Calculating financial outcome
  gain_TP <- TP * 50
  gain_TN <- TN * 20
  loss_FP <- FP * (-20)
  loss_FN <- FN * (-50)
  tot_outcome <- gain_TP + gain_TN + loss_FP + loss_FN
  
  # Calculating metrics
  accuracy <- (TP + TN) / sum(conf_matrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * (precision * recall) / (precision + recall)
  
  cat("Threshold: ", tt, "\n",
      "Financial Gain: ", tot_outcome, "\n",
      "Accuracy: ", accuracy, "\n",
      "Precision: ", precision, "\n",
      "Recall (Sensitivity): ", recall, "\n",
      "F1 Score: ", F1_score, "\n\n")
}

# Calculate metrics for each threshold
for (tt in thresholds) {
  calculate_metrics(tt)
}

# Calculate the ROC curve
roc <- roc(response =val_data$Closed_Account,
           predictor = predict(logit_fit_bic1,
                               newdata = val_data, type = "response"), 
           levels = c("0", "1"))
auc_value <- auc(roc)
print(paste("AUC:", auc_value))

# Create a data frame from the roc object for plotting
roc_data <- data.frame(
  TPR = roc$sensitivities,
  FPR = roc$specificities,
  Thresholds = roc$thresholds
)

# Plot the ROC curve
ggplot(roc_data, aes(x = 1 - FPR, y = TPR)) + 
  geom_line(color = "#1c61b6", size = 1) +
  geom_area(fill = "#1c61b624") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  annotate("text", x = 0.8, y = 0.2, label = paste("AUC:", round(auc_value, 3)),
           hjust = 0, color = "#1c61b6") +
  theme_minimal() +
  labs(
    title = "ROC Curve",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


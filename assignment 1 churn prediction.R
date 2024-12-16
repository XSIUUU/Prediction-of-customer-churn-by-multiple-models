# summary
summary(churn)

# Data wrangling of home labeling
install.packages("dplyr")
library(dplyr)

churn <- churn %>% 
  mutate(Home_label = case_when(
    Home_label == "G" ~ 1,
    Home_label == "F" ~ 2,
    Home_label == "E" ~ 3,
    Home_label == "D" ~ 4,
    Home_label == "C" ~ 5,
    Home_label == "B" ~ 6,
    Home_label == "A" ~ 7
  )) %>% 
  mutate(contract_type = ifelse(Contract_length>=1,1,0))

# data cleaning----
# income (50000 as outliers identification)
churn <- churn %>% mutate(income_type = ifelse(Income>50000,1,0)) 
churn %>% group_by(income_type) %>% summarize(mean_income=mean(Income))
churn <- churn %>% mutate(Income = 
                            case_when(income_type==1 ~ Income/12,
                                      TRUE ~ Income))

# electricity usage (5000 as outliers identification)
churn <- churn %>% mutate(Electricity_usage_type = ifelse(Electricity_usage>5000,1,0)) 
churn %>% group_by(Electricity_usage_type) %>% summarize(mean_electricity_usage=mean(Electricity_usage))
churn <- churn %>% mutate(Electricity_usage = 
                            case_when(Electricity_usage_type==1 ~ Electricity_usage/12,
                                      TRUE ~ Electricity_usage))

# gas usage (5000 as outliers identification)
churn <- churn %>% mutate(gas_usage_type = ifelse(Gas_usage>5000,1,0)) 
churn %>% group_by(gas_usage_type) %>% summarize(mean_gas_usage=mean(Gas_usage))
churn <- churn %>% mutate(Gas_usage = 
                            case_when(gas_usage_type==1 ~ Gas_usage/12,
                                      TRUE ~ Gas_usage))



# summary after wrangling
summary(churn)

# visualization
library(ggplot2)

hist(churn$Age,main="Age")

ggplot(churn, aes(x = "", y = Income)) +
  geom_violin(fill = "lightblue") +
  labs(title = "Income Violin Plot", x = "", y = "Income") +
  theme_minimal()
boxplot(churn$Income,main="Income")  ###  ?? do we need to separate the income into groups

plot(churn$Income,main="Income")

hist(churn$contract_type,main="Contract_type")

hist(churn$Home_label,main="Home_label", breaks = seq(0.5, 7.5, by = 1))

boxplot(churn$Electricity_usage,main="Electricity_usage")
boxplot(churn$Gas_usage,main="Gas_usage")

hist(churn$Churn,main="Churn")


# correlation
cor.test(churn$Churn, churn$Age, method = "pearson")
cor.test(churn$Churn, churn$Income, method = "pearson")
cor.test(churn$Churn, churn$contract_type, method = "pearson")
cor.test(churn$Churn, churn$Home_label, method = "pearson")
cor.test(churn$Churn, churn$Electricity_usage, method = "pearson")

# visual of corr
install.packages("corrplot") 
library(corrplot)

corrplot.mixed(cor(churn[,c(14,3,4,10,11,15)]), upper = "ellipse")

# visual of iv and dv----
#age
plot(jitter(churn$Churn), jitter(churn$Age))
boxplot(churn[churn$Churn==0,"Age"],churn[churn$Churn==1,"Age"],
        names = c("Unchurn", "Churn"),main="Age Distribution by Churn Status")
#income
plot(jitter(churn$Churn), jitter(churn$Income))
boxplot(churn[churn$Churn==0,"Income"],churn[churn$Churn==1,"Income"],
        names = c("Unchurn", "Churn"),main="Income Distribution by Churn Status")
#Home_label
plot(jitter(churn$Churn), jitter(churn$Home_label))
boxplot(churn[churn$Churn==0,"Home_label"],churn[churn$Churn==1,"Home_label"],
        names = c("Unchurn", "Churn"),main="Home_label Distribution by Churn Status")
#Electricity_usage
plot(jitter(churn$Churn), jitter(churn$Electricity_usage))
boxplot(churn[churn$Churn==0,"Electricity_usage"],churn[churn$Churn==1,"Electricity_usage"],
        names = c("Unchurn", "Churn"),main="Electricity_usage Distribution by Churn Status")
#contract type
churnsample <- churn[sample(1:nrow(churn), 250, replace=FALSE),]
plot(jitter(churnsample$Churn), jitter(churnsample$contract_type),
     xlab = "Churn Status", 
     ylab = "Contract Type",
     main = "Scatter Plot of Churn vs Contract Type",
     xaxt = "n",  
     yaxt = "n")  
# Customize x-axis and y-axis labels
axis(1, at = c(0, 1), labels = c("Unchurn", "Churn"))  
axis(2, at = c(0, 1), labels = c("Flexible", "Long-term"))  

# Statistic test----
## pre test----
####Test for normality (p<0.05, reject h0, not normality)
library(nortest)
#age
lillie.test(churn[churn$Churn==0,"Age"])
lillie.test(churn[churn$Churn==1,"Age"])
#income
lillie.test(churn[churn$Churn==0,"Income"])
lillie.test(churn[churn$Churn==1,"Income"])
#Home_label
lillie.test(churn[churn$Churn==0,"Home_label"])
lillie.test(churn[churn$Churn==1,"Home_label"])
#Electricity_usage
lillie.test(churn[churn$Churn==0,"Electricity_usage"])
lillie.test(churn[churn$Churn==1,"Electricity_usage"])

## Wilcoxon test----
wilcox.test(Age ~ Churn, data = churn)
wilcox.test(Income ~ Churn, data = churn)
wilcox.test(Home_label ~ Churn, data = churn)
wilcox.test(Electricity_usage ~ Churn, data = churn)

#chisq for contract type
chisq.test(churn$Churn,churn$contract_type)
install.packages("gmodels")
library(gmodels)
CrossTable(churn$Churn,churn$contract_type)


#---------------------------
# 6.1 logistic regression 
#---------------------------

churn$contract_type <- as.factor(churn$contract_type)
model <- glm(Churn ~ Age + Income + Home_label + contract_type + Electricity_usage, data = churn, family = binomial)
summary(model)
# Odds ratios
exp(coef(model))

# Predicted probabilities
predictions_model <- predict(model, type = "response", newdata=churn)

# Fit criteria ------------------------------------------------------------


#Make the basis for the hit rate table
predicted_model <- ifelse(predictions_model>.5,1,0)

hit_rate_model <- table(churn$Churn, predicted_model, dnn= c("Observed", "Predicted"))

hit_rate_model

#Get the hit rate
(hit_rate_model[1,1]+hit_rate_model[2,2])/sum(hit_rate_model)


#Top decile lift
library(dplyr) 

decile_predicted_model <- ntile(predictions_model, 10)

decile_model <- table(churn$Churn, decile_predicted_model, dnn= c("Observed", "Decile"))

decile_model

#Calculate the TDL
(decile_model[2,10] / (decile_model[1,10]+ decile_model[2,10])) / mean(churn$Churn)


#Make lift curve
install.packages("ROCR")
library(ROCR)

pred_model <- prediction(predictions_model, churn$Churn)
perf_model <- performance(pred_model,"tpr","fpr")
plot(perf_model,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model <- performance(pred_model,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model@y.values)*2-1



# Out of sample validation ------------------------------------------------

#Get a 75% estimation sample and 25% validation sample
set.seed(1234)
churn$estimation_sample <-rbinom(nrow(churn), 1, 0.75)


#Estimate the model using only the estimation sample
Logistic_regression2 <- glm(Churn ~ Age + Income + Home_label + contract_type + Electricity_usage, family=binomial, data=churn, subset=estimation_sample==1)


#Create a new dataframe with only the validation sample
our_validation_dataset <- churn[churn$estimation_sample==0,]

#Get predictions for all observations
predictions_model2 <- predict(Logistic_regression2, type = "response", newdata= our_validation_dataset)

### After this you can calculate the fit criteria on this validation sample
predicted_model2 <- ifelse(predictions_model2>.5,1,0)

hit_rate_model2 <- table(our_validation_dataset$Churn, predicted_model2, dnn= c("Observed", "Predicted"))

hit_rate_model2

#Get the hit rate
(hit_rate_model2[1,1]+hit_rate_model2[2,2])/sum(hit_rate_model2)


#Top decile lift
library(dplyr) 

decile_predicted_model2 <- ntile(predictions_model2, 10)

decile_model2 <- table(our_validation_dataset$Churn, decile_predicted_model2, dnn= c("Observed", "Decile"))

decile_model2

#Calculate the TDL
(decile_model2[2,10] / (decile_model2[1,10]+ decile_model2[2,10])) / mean(churn$Churn)


#Make lift curve
install.packages("ROCR")
library(ROCR)

pred_model2 <- prediction(predictions_model2, our_validation_dataset$Churn)
perf_model2 <- performance(pred_model2,"tpr","fpr")
plot(perf_model2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model2 <- performance(pred_model2,"auc")

#The Gini is related to the "Area under the Curve" (AUC), namely by: Gini = AUC*2 - 1
#So to get the Gini we do:
as.numeric(auc_model2@y.values)*2-1

#-----------------------------
# 6.2 Stepwise logistic regression 
#------------------------------
library(MASS) 
# Estimate full and null model
Logistic_regression_full <- glm(Churn ~ ., data = churn, family = binomial, subset = estimation_sample == 1)
Logistic_regression_null <- glm(Churn ~ 0, data = churn, family = binomial, subset = estimation_sample == 1)

# Fit the model backward
Logistic_regression_backward <- stepAIC(Logistic_regression_full, direction = "backward", trace = TRUE)

# Fit the model forward
Logistic_regression_forward <- stepAIC(Logistic_regression_null, direction = "forward", 
                                       scope = list(lower = Logistic_regression_null, upper = Logistic_regression_full), 
                                       trace = TRUE)

# Fit the model both directions
Logistic_regression_both <- stepAIC(Logistic_regression_full, direction = "both", trace = TRUE)

## To do step-wise regression with the BIC you can add "k = log(n)" (where n is the amount of observations on which the model is estimated), example:

# Fit the model backwards/forwards/both ways using BIC
Logistic_regression_backward_BIC <- stepAIC(Logistic_regression_full, direction = "backward", 
                                            trace = TRUE, k = log(sum(churn$estimation_sample)))

Logistic_regression_forward_BIC <- stepAIC(Logistic_regression_full, direction = "forward", 
                                           trace = TRUE, k = log(sum(churn$estimation_sample)))

Logistic_regression_both_BIC <- stepAIC(Logistic_regression_full, direction = "both", 
                                        trace = TRUE, k = log(sum(churn$estimation_sample)))



## Fit criteria ------------------------------------------------------------

## Fit criteria Stepwise regression ------------------------------------------------------------

# Get predictions from all the regression models
predictions_model_forward <- predict(Logistic_regression_forward, type = "response", newdata = our_validation_dataset)
predictions_model_backward <- predict(Logistic_regression_backward, type = "response", newdata = our_validation_dataset)
predictions_model_both <- predict(Logistic_regression_both, type = "response", newdata = our_validation_dataset)
predictions_model_forward_BIC <- predict(Logistic_regression_forward_BIC, type = "response", newdata = our_validation_dataset)
predictions_model_backward_BIC <- predict(Logistic_regression_backward_BIC, type = "response", newdata = our_validation_dataset)
predictions_model_both_BIC <- predict(Logistic_regression_both_BIC, type = "response", newdata = our_validation_dataset)

# Calculate evaluation metrics
validation_model_forward <- validate_model(our_validation_dataset$Churn, predictions_model_forward)
validation_model_backward <- validate_model(our_validation_dataset$Churn, predictions_model_backward)
validation_model_both <- validate_model(our_validation_dataset$Churn, predictions_model_both)
validation_model_forward_BIC <- validate_model(our_validation_dataset$Churn, predictions_model_forward_BIC)
validation_model_backward_BIC <- validate_model(our_validation_dataset$Churn, predictions_model_backward_BIC)
validation_model_both_BIC <- validate_model(our_validation_dataset$Churn, predictions_model_both_BIC)
validation_model_forward
validation_model_backward
validation_model_both
validation_model_forward_BIC
validation_model_backward_BIC
validation_model_both_BIC



# --------------------------------
# 6.3 Cart Trees
# --------------------------------
# Load required libraries
install.packages("rpart")
install.packages("partykit")
library(rpart)
library(partykit)
# Build CART decision tree model including all variables
Cart_tree_full <- rpart(
  Churn ~ Gender + Age + Income + Relation_length + Contract_length + 
    Start_channel + Email_list + Home_age + Home_label + contract_type +
    Electricity_usage + Gas_usage + Province,
  data = churn,
  method = "class"
)
# --------------------------------
# 6.3.1 Cart_tree_custom
# --------------------------------
# Visualize the decision tree
Cart_tree_full_visual <- as.party(Cart_tree_full)
plot(Cart_tree_full_visual, type = "simple", gp = gpar(fontsize = 10))
# Adjust model parameters to control tree complexity
newsettings <- rpart.control(minsplit = 100, minbucket = 50, cp = 0.01, maxdepth = 3)
# Create CART decision tree model with custom settings
Cart_tree_custom <- rpart(
  Churn ~ Gender + Age + Income + Relation_length + Contract_length + 
    Start_channel + Email_list + Home_age + Home_label + contract_type +
    Electricity_usage + Gas_usage + Province,
  data = churn,
  method = "class",
  control = newsettings
)
# Visualize the customized decision tree
Cart_tree_custom_visual <- as.party(Cart_tree_custom)
plot(Cart_tree_custom_visual, type = "simple")
# --------------------------------
#6.3.2 Cart_tree_custom - importance pic with ggplot
# --------------------------------
# Evaluate the model performance
printcp(Cart_tree_full)  # Print complexity parameter table
plotcp(Cart_tree_full)   # Plot cross-validated error to visualize pruning
# Feature importance analysis
install.packages("caret")
library(caret)
importance_custom <- varImp(Cart_tree_full, scale = FALSE)
print(importance_custom)
# Convert significance data to dataframe format suitable for ggplot2
importance_df <- data.frame(
  Feature = rownames(importance_custom),
  Importance_custom = importance_custom$Overall
)
# Plot bar charts of feature importance
ggplot(importance_df, aes(x = Importance_custom, y = reorder(Feature, Importance_custom))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Feature Importance for CART Custom",
       x = "Importance",
       y = "Features") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



# --------------------------------
# 6.3.3 Cart_tree_optimized
# --------------------------------
# Adjustment of model parameters
newsettings1 <- rpart.control(minsplit = 50, minbucket = 25, cp = 0.005, maxdepth = 5)
Cart_tree_optimized <- rpart(
  Churn ~ Gender + Age + Income + Relation_length + Contract_length + 
    Start_channel + Email_list + Home_age + Home_label + contract_type +
    Electricity_usage + Gas_usage + Province,
  data = churn,
  method = "class",
  control = newsettings1
)
# Evaluate performance
printcp(Cart_tree_optimized)
plotcp(Cart_tree_optimized)
# Characterization importance analysis
importance <- varImp(Cart_tree_optimized, scale = FALSE)
print(importance)
# Visualize the optimized decision tree
library(partykit)
Cart_tree_optimized_visual <- as.party(Cart_tree_optimized)
plot(Cart_tree_optimized_visual, type = "simple", gp = gpar(fontsize = 10))
# --------------------------------
# 6.3.4 Cart_tree_optimized- importance pic with ggplot
# --------------------------------
# Get the feature importance for the CART Optimized model
library(caret)
importance_optimized <- varImp(Cart_tree_optimized, scale = FALSE)
# Convert the importance data to a dataframe suitable for ggplot2
importance_df_optimized <- data.frame(
  Feature = rownames(importance_optimized),
  Importance = importance_optimized$Overall
)
# Use ggplot to create a bar plot
library(ggplot2)
ggplot(importance_df_optimized, aes(x = Importance, y = reorder(Feature, Importance))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(
    title = "Feature Importance for CART Optimized",
    x = "Importance",
    y = "Features"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



# --------------------------------
# 6.3.5 comparation between custom and optimized model
# --------------------------------
# 1. Define the validate_model function to calculate evaluation metrics
validate_model <- function(true_values, predicted_probabilities) {
  # Convert predicted probabilities to binary outcomes
  predicted_class <- ifelse(predicted_probabilities > 0.5, 1, 0)
  # Create confusion matrix
  hit_rate_table <- table(true_values, predicted_class, dnn = c("Observed", "Predicted"))
  # Calculate hit rate
  hit_rate <- (hit_rate_table[1, 1] + hit_rate_table[2, 2]) / sum(hit_rate_table)
  # Calculate top decile lift
  decile <- ntile(predicted_probabilities, 10)
  decile_table <- table(true_values, decile, dnn = c("Observed", "Decile"))
  top_decile_lift <- (decile_table[2, 10] / (decile_table[1, 10] + decile_table[2, 10])) / mean(true_values)
  # Calculate Gini
  pred_model <- prediction(predicted_probabilities, true_values)
  auc <- performance(pred_model, "auc")
  gini <- as.numeric(auc@y.values) * 2 - 1
  # Return results
  return(list(
    hit_rate = hit_rate,
    top_decile_lift = top_decile_lift,
    gini = gini
  ))
}

# 2. Create estimation and validation samples
set.seed(1234)
churn$estimation_sample <- rbinom(nrow(churn), 1, 0.75)

# 3. Create validation dataset
our_validation_dataset <- churn[churn$estimation_sample == 0,]

# 4. Get predictions from the CART Custom and CART Optimized models
# I already have this part, so I won't repeat it
# 5. Get predictions for the CART Custom and CART Optimized models
predictions_cart_custom <- predict(Cart_tree_custom, newdata = our_validation_dataset, type = "prob")[, 2]
predictions_cart_optimized <- predict(Cart_tree_optimized, newdata = our_validation_dataset, type = "prob")[, 2]

# 6. Calculate evaluation metrics
validation_cart_custom <- validate_model(our_validation_dataset$Churn, predictions_cart_custom)
validation_cart_optimized <- validate_model(our_validation_dataset$Churn, predictions_cart_optimized)

# 7. Combine all model evaluation results
validation_results <- data.frame(
  Model = c("CART Custom", "CART Optimized"),
  Hit_Rate = c(validation_cart_custom$hit_rate, validation_cart_optimized$hit_rate),
  Top_Decile_Lift = c(validation_cart_custom$top_decile_lift, validation_cart_optimized$top_decile_lift),
  Gini = c(validation_cart_custom$gini, validation_cart_optimized$gini)
)

# 8. Print the evaluation results
print(validation_results)

#----------------------
# 6.4 bagging
#---------------------
# Bagging -----------------------------------------------------------------
library(ipred)
library(caret)
library(rpart)

newsettings2 <- rpart.control(minsplit = 2, cp = 0.0)
#Essentially these two arguments allow the individual trees to grow extremely deep, which leads to trees with high variance but low bias. Then when we apply bagging we're able to reduce the variance of the final model while keeping the bias low.

#estimate model with bagging
Bagging_tree1 <- bagging(as.factor(Churn) ~ Gender + Age + Income + Relation_length + Contract_length + 
                           Start_channel + Email_list + Home_age + Home_label + 
                           Electricity_usage + Gas_usage + Province + contract_type,
                         data=churn, method="treebag", nbagg=500, coob=TRUE, subset=estimation_sample==1, control=newsettings2)

#Save predictions
predictions_bagging1 <- predict(Bagging_tree1, newdata=our_validation_dataset, type ="prob")[,2]

#validate check
validate_model(our_validation_dataset$Churn,predictions_bagging1)

#calculate variable importance...
pred.imp <- varImp(Bagging_tree1)
pred.imp


#You can also plot the results
bar_positions <- barplot(pred.imp$Overall, 
                         names.arg = NULL, 
                         cex.names = 0.8)

# Add 45 degree rotated labels
text(x = bar_positions, 
     y = -0.5, 
     labels = row.names(pred.imp), 
     srt = 45, 
     adj = 1, 
     xpd = TRUE, 
     cex = 1) 


#-----------------------
# 6.5 boosting
#-----------------------
library(gbm)
estimation_sample <- churn[churn$estimation_sample==1,]

#Estimate the model
boost_tree1 <- gbm(Churn ~ as.factor(Gender) + Age + Income + Relation_length + Contract_length + 
                     as.factor(Start_channel) + as.factor(Email_list) + Home_age + Home_label + 
                     Electricity_usage + Gas_usage + as.factor(Province) + as.factor(contract_type),
                   data=churn, distribution = "bernoulli", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)

#Get model output (summary also provides a graph)
boost_tree1

best.iter <- gbm.perf(boost_tree1, method = "OOB")
best.iter
summary(boost_tree1, n.trees = best.iter)

#visualization
importance_data <- summary(boost_tree1, n.trees = best.iter, plotit = FALSE)


bar_positions <- barplot(
  importance_data$rel.inf, 
  names.arg = NULL, 
  main = "Variable Importance", 
  xlab = "Variables", 
  ylab = "Relative Influence", 
  las = 2, 
  cex.names = 0.8, 
  col = "lightblue"
)

# Add a 30-degree variables label
text(
  x = bar_positions, 
  y = -0.5, 
  labels = importance_data$var, 
  srt = 30, 
  adj = 1, 
  xpd = TRUE, 
  cex = 0.8 
)

#Save predictions
predictions_boost1 <- predict(boost_tree1, newdata=our_validation_dataset, n.trees = best.iter, type ="response")

#validate check
validate_model(our_validation_dataset$Churn,predictions_boost1)






# --------------------------------
# 6.6 Random Forest -----------------------------------------------------------
# --------------------------------
library(randomForest)

# Train Random Forest model using estimation sample
Random_forest1 <- randomForest(as.factor(Churn) ~ Gender + Age + Income + Relation_length + Contract_length + contract_type +
                                 Start_channel + Email_list + Home_age + Home_label + 
                                 Electricity_usage + Gas_usage + Province, 
                               data = churn, subset = estimation_sample == 1, importance = TRUE)

# Get predictions on the validation dataset
predictions_forest1 <- predict(Random_forest1, newdata = our_validation_dataset, type = "prob")[, 2]

# Plot feature importance
varImpPlot(Random_forest1)

# Train Random Forest with extra settings (tune hyperparameters)
Random_forest1_tuned <- randomForest(as.factor(Churn) ~ Gender + Age + Income + Relation_length + Contract_length + contract_type +
                                       Start_channel + Email_list + Home_age + Home_label + 
                                       Electricity_usage + Gas_usage + Province, 
                                     data = churn, subset = estimation_sample == 1, 
                                     ntree = 500, mtry = 3, nodesize = 1, maxnodes = 100, importance = TRUE)

# Get predictions for the tuned Random Forest model
predictions_forest1_tuned <- predict(Random_forest1_tuned, newdata = our_validation_dataset, type = "prob")[, 2]

# Validate the Random Forest models using the 'validate_model' function
validation_forest1 <- validate_model(our_validation_dataset$Churn, predictions_forest1)
validation_forest1_tuned <- validate_model(our_validation_dataset$Churn, predictions_forest1_tuned)

# Combine the validation results into a summary dataframe
validation_results_rf <- data.frame(
  Model = c("Random Forest", "Random Forest Tuned"),
  Hit_Rate = c(validation_forest1$hit_rate, validation_forest1_tuned$hit_rate),
  Top_Decile_Lift = c(validation_forest1$top_decile_lift, validation_forest1_tuned$top_decile_lift),
  Gini = c(validation_forest1$gini, validation_forest1_tuned$gini)
)

# Print the validation results
print(validation_results_rf)


#-----------------------------------
# 6.7 Support Vector Machine 
#-----------------------------------
# Load necessary library
library(e1071)

# Ensure all variables are correctly prepared
# Drop irrelevant columns and ensure all predictors are numeric
churn <- churn %>%
  mutate(across(where(is.character), ~ as.numeric(as.factor(.))),
         across(where(is.factor), ~ as.numeric(as.character(.))))

# Split dataset into estimation and validation samples
set.seed(1234)
churn$estimation_sample <- rbinom(nrow(churn), 1, 0.75)

# Define the estimation dataset
estimation_sample <- churn[churn$estimation_sample == 1, ]

# Fit the SVM model
svm_model_churn_age_income_radial <- svm(Churn ~ Age + Income, ## This can be changed to any of 2 variables we want to discover
                                         data = churn[,1:15],
                                         subset=churn$estimation_sample==1,
                                         type = 'C-classification', 
                                         probability = TRUE,
                                         kernel = 'radial')

svm_model_churn_age_income_sigmoid <- svm(Churn ~ Age + Income, 
                                          data = churn[,1:15],
                                          subset=churn$estimation_sample==1,
                                          type = 'C-classification', 
                                          probability = TRUE,
                                          kernel = 'sigmoid')

plot(svm_model_churn_age_income_radial, churn, Age~Income)
plot(svm_model_churn_age_income_sigmoid, churn, Age~Income)

svm_model_churn_all_radial <- svm(Churn ~ Age + Income + Relation_length + Contract_length + Email_list +
                                    Home_age + Electricity_usage + Gas_usage + Home_label, ## uses all the important variables which could be interperted for hypothesis testing
                                  data = churn[,1:15],
                                  subset=churn$estimation_sample==1,
                                  type = 'C-classification', 
                                  probability = TRUE,
                                  kernel = 'radial')
svm_model_churn_all_sigmoid <- svm(Churn ~ Age + Income + Relation_length + Contract_length + Email_list +
                                     Home_age + Electricity_usage + Gas_usage + Home_label, 
                                   data = churn[,1:15],
                                   subset=churn$estimation_sample==1,
                                   type = 'C-classification', 
                                   probability = TRUE,
                                   kernel = 'sigmoid')

plot(svm_model_churn_all_radial, churn, Age~Income)
plot(svm_model_churn_all_radial, churn, Gas_usage~Electricity_usage)
plot(svm_model_churn_all_radial, churn, Home_age~Home_label)
plot(svm_model_churn_all_radial, churn, Relation_length~Contract_length)
plot(svm_model_churn_all_sigmoid, churn, Age~Income)
plot(svm_model_churn_all_sigmoid, churn, Gas_usage~Electricity_usage)
plot(svm_model_churn_all_sigmoid, churn, Home_age~Home_label)
plot(svm_model_churn_all_sigmoid, churn, Relation_length~Contract_length)

## Fit criteria SVM -------------------------------- 

### Get predictions SVM's ----
predictions_svm_age_income_radial <- predict(svm_model_churn_age_income_radial, newdata=our_validation_dataset, probability=TRUE)
predictions_svm_age_income_sigmoid <- predict(svm_model_churn_age_income_sigmoid, newdata=our_validation_dataset, probability=TRUE)
predictions_svm_age_income_radial <- attr(predictions_svm_age_income_radial,"probabilities")[,1]
predictions_svm_age_income_sigmoid <- attr(predictions_svm_age_income_sigmoid,"probabilities")[,1]
plot(predictions_svm_age_income_sigmoid)
plot(predictions_svm_age_income_radial)

predictions_svm_churn_all_radial <- predict(svm_model_churn_all_radial, newdata=our_validation_dataset, probability=TRUE)
predictions_svm_churn_all_sigmoid <- predict(svm_model_churn_all_sigmoid, newdata=our_validation_dataset, probability=TRUE)
predictions_svm_churn_all_radial <- attr(predictions_svm_churn_all_radial,"probabilities")[,1]
predictions_svm_churn_all_sigmoid <- attr(predictions_svm_churn_all_sigmoid,"probabilities")[,1]
plot(predictions_svm_churn_all_radial)
plot(predictions_svm_churn_all_sigmoid)

###Calculate evaluation metrics ----
validation_model_svm_age_income_radial <- validate_model(our_validation_dataset$Churn, predictions_svm_age_income_radial)
validation_model_svm_age_income_sigmoid <- validate_model(our_validation_dataset$Churn, predictions_svm_age_income_sigmoid)
validation_model_svm_churn_all_radial <- validate_model(our_validation_dataset$Churn, predictions_svm_churn_all_radial)
validation_model_svm_churn_all_sigmoid <- validate_model(our_validation_dataset$Churn, predictions_svm_churn_all_sigmoid)
validation_model_svm_age_income_radial
validation_model_svm_age_income_sigmoid
validation_model_svm_churn_all_radial
validation_model_svm_churn_all_sigmoid

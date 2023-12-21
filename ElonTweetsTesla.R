
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")

install.packages("tidymodels")
install.packages("textrecipes")
install.packages("randomForest")

install.packages("broom")
install.packages("caret")
install.packages("pROC")

install.packages("glmnet")


library(dplyr)
library(tidyverse)
library(lubridate)

library(tidymodels)
library(textrecipes)
library(randomForest)

library(broom)      # For tidying model outputs
library(caret)

library(pROC)

library(ggplot2)

csv_files <- list.files(path="tweets",pattern= ".csv", full.names=TRUE)
csv_files
needed_parameters <- c("id", "date", "tweet", "hashtags", "cashtags", "username", "link", "retweet", "nlikes", "nreplies", "nretweets", "retweet_id", "retweet_date")
needed_parameters
tweet_data <- list()
for (file in csv_files) {
  #print(file) 
  df <- read.csv(file)
  df_cleaned <- df[, colnames(df) %in% needed_parameters]
  #print(df_cleaned)
  tweet_data[[file]] <- df_cleaned
}

#
merged_df <- bind_rows(tweet_data)

tweet_data <- merged_df
tweet_data <- tweet_data %>% distinct(link, .keep_all = TRUE)

stock_data <- read.csv("tesla/tesla20102022.csv")

stock_data$date <- as.Date(stock_data$date)
tweet_data$date <- as.Date(tweet_data$date)
tweet_data <- tweet_data %>% filter(date >= as.Date("2015-01-01") & date < as.Date("2022-01-01"))
stock_data <- stock_data %>% filter(date >= as.Date("2015-01-01") & date < as.Date("2022-01-01"))

merged_data <- merge(tweet_data, stock_data, by = "date")

merged_data$price_movement <- ((merged_data$close - merged_data$open) / merged_data$open) * 100

data <- merged_data

data_grouped <- data %>%
  group_by(date)

# If you want to include multiple summaries
daily_summary <- data_grouped %>%
  summarise(
    total_likes = sum(nlikes, na.rm = TRUE),
    total_replies = sum(nreplies, na.rm = TRUE),
    total_retweets = sum(nretweets, na.rm = TRUE),
    volume = mean(volume, na.rm = TRUE),
    price_movement = mean(price_movement, na.rm = TRUE)
  )

# First, create a target binary variable for stock movement: 1 for increase, 0 for decrease/no change
daily_summary$price_movement_binary <- ifelse(daily_summary$price_movement > 0, 1, 0)

daily_summary_long <- daily_summary %>%
  pivot_longer(cols = c(total_likes, total_replies, total_retweets, volume), 
               names_to = "metric", values_to = "value")

# Now, create a scatter plot with faceting
ggplot(daily_summary_long, aes(x = value, y = price_movement)) +
  geom_point(aes(color = metric)) +  # Color points by metric for clarity
  facet_wrap(~ metric, scales = "free_x") +  # Create a separate plot for each metric
  labs(title = "Price Movement vs. Various Metrics",
       x = "Metric Value",
       y = "Price Movement (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# To see the result
print(daily_summary)

#correlation between sum of tweet likes in a day and TSLA stock price movement
cor(daily_summary$total_likes, daily_summary$price_movement, use = "complete.obs")

# Select features
features <- daily_summary %>%
  select(total_likes, total_replies, total_retweets, price_movement_binary)

# Split data into training and test sets
set.seed(123) # For reproducibility


# Create the partition
training_indices <- createDataPartition(features$price_movement_binary, p = 0.8, list = FALSE)

# Split the data
train_data <- features[training_indices, ]
test_data <- features[-training_indices, ]

test_data

# Fit the logistic regression model
logit_model <- glm(price_movement_binary ~ ., data = train_data, family = binomial)

# Summary of the model to see coefficients and significance
summary(logit_model)

# Predict on the test set
test_data$predicted_class <- predict(logit_model, newdata = test_data, type = "response")


# Optionally, calculate and plot ROC curve

roc_curve <- roc(test_data$price_movement_binary, test_data$predicted_class)
plot(roc_curve)


test_data$predicted_class <- ifelse(test_data$predicted_class > 0.5, "Increase", "Decrease")
test_data$predicted_class <- factor(test_data$predicted_class, levels = c("Increase", "Decrease"))

test_data$price_movement_binary <- ifelse(test_data$price_movement_binary > 0, "Increase", "Decrease")
test_data$price_movement_binary <- factor(test_data$price_movement_binary, levels = c("Increase", "Decrease"))

test_data
# Evaluate model performance
conf_matrix <- confusionMatrix(test_data$predicted_class, test_data$price_movement_binary)
print(conf_matrix)

# Sensitivity (also called the true positive rate or recall) is quite high at 0.8613, meaning the model is good at identifying the positive class.
# Specificity is very low at 0.2000, indicating that the model is not good at identifying the negative class.
# Positive Predictive Value (PPV) and Negative Predictive Value (NPV) are around 0.54 and 0.56, respectively, which are not much better than a random guess.
# Balanced Accuracy, which is the average of sensitivity and specificity, is 0.5307, just slightly better than a random guess, suggesting that the model's ability to classify both positive and negative classes is not impressive.


###################### LASSO REGRESSION #####################


library(glmnet)

features <- daily_summary %>%
  select(total_likes, total_replies, total_retweets, price_movement_binary)
features

# Split data into training and test sets
set.seed(123) # For reproducibility

# Create the partition
training_indices <- createDataPartition(features$price_movement_binary, p = 0.8, list = FALSE)

# Split the data
train_data <- features[training_indices, ]
test_data <- features[-training_indices, ]

# Assuming your features are in a dataframe called 'train_data'
# and the outcome variable is called 'price_movement_binary'

# Prepare the matrix of predictors and the response variable.
# Exclude the date variable and any other non-numeric predictors
x <- model.matrix(price_movement_binary ~ . , train_data)[,-1]  # -1 to exclude intercept
y <- train_data$price_movement_binary

# glmnet requires a numeric matrix/vector input, so convert y to numeric if it's a factor
if (is.factor(y)) {
  y <- as.numeric(as.character(y)) - 1  # converting factor to numeric (0/1 for binary)
}

# Fit the Lasso model
set.seed(123)  # For reproducibility
cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv_lasso)

# Find the best lambda (regularization parameter)
best_lambda <- cv_lasso$lambda.min

# Now fit the model using the best lambda
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = "binomial")
# Coefficients across a range of lambdas
coef(lasso_model)
# Intercept ((Intercept)): This is the baseline prediction when all other predictors are zero. The value 8.268400e-02 represents the log-odds of the outcome being 'Increase' when all other predictors are zero, since Lasso regression in glmnet is fitted using log-odds for binomial outcomes.
# 
# Total Likes (total_likes): The coefficient for total_likes has been shrunk to zero. This suggests that within the context of the Lasso model and the data provided, total_likes is not contributing to the prediction of price_movement_binary.
# 
# Total Replies (total_replies): Similarly, the coefficient for total_replies is zero, indicating no contribution to the prediction of price_movement_binary.
# 
# Total Retweets (total_retweets): The coefficient for total_retweets is very small and negative (-2.547782e-06). This indicates a very slight negative relationship with the outcome, suggesting that as total_retweets increases, the log-odds of the price movement being 'Increase' decreases, but the effect is minimal.



# Predict on test data
x_test <- model.matrix(~ ., test_data)[,-1]

dim(x)
dim(x_test)
x_test <- model.matrix(~ . - 1, test_data)

predictors_in_model <- rownames(lasso_model$beta) %>%
  na.omit() %>%
  as.character()
x_test <- x_test[, predictors_in_model, drop = FALSE]

predictions <- predict(lasso_model, newx = x_test, type = "response")
test_data$predicted_class <- factor(test_data$predicted_class, levels = c("Decrease", "Increase"))

test_data$price_movement_binary <- ifelse(test_data$price_movement_binary > 0, "Increase", "Decrease")
test_data$price_movement_binary <- as.factor(test_data$price_movement_binary)
# Evaluate the model
conf_matrix <- confusionMatrix(test_data$predicted_class, test_data$price_movement_binary)
print(conf_matrix)

# Optionally, plot the coefficients
plot(lasso_model, xvar = "lambda", label = TRUE)


####################### RANDOM FOREST #######################
split <- initial_split(features, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

# Train the Random Forest model
rf_spec <- rand_forest(trees = 100) %>%
  set_mode("classification") %>%
  set_engine("randomForest")

# Fit model using a formula that specifies our binary outcome and all other columns as predictors
train_data$price_movement_binary <- as.factor(train_data$price_movement_binary)

test_data$price_movement_binary <- as.factor(test_data$price_movement_binary)
# Now, fit the model with the outcome as a factor
rf_fit <- rf_spec %>%
  fit(price_movement_binary ~ ., data = train_data)

# Now retry prediction
rf_results <- rf_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = price_movement_binary, estimate = .pred_class)

rf_results

# The accuracy is the proportion of the total number of predictions that were correct. In this case, it's 0.494, or 49.4%, which indicates that the model is correct about half the time.
# The kappa statistic (kap) measures the agreement between the predicted and actual classifications, corrected for the agreement that could happen by chance. A kappa value near 0 (0.000514 in this case) indicates that there is hardly any agreement between the predictions and the actuals other than what would be expected by chance.
# Given this information, here is what you can interpret:
# 
# The model's performance is close to random guessing on your dataset, as indicated by an accuracy of 49.4%, which is nearly a 50/50 chance. This could suggest that the model is not capturing the patterns in the data effectively, or that the data doesn't contain strong signals for the model to learn from.
# 
# The kappa statistic being close to zero further confirms that the model is not performing well. A good model should have a kappa statistic significantly higher than zero.


#Interpretation

confusion_matrix <- confusionMatrix(rf_results$pred, rf_results$obs)
print(confusion_matrix)

predicted_probabilities <- predict(rf_fit, test_data, type = "prob")

roc_curve <- roc(predicted_probabilities$.pred_0, predicted_probabilities$.pred_0)
autoplot(roc_curve)


model_importance <- importance(rf_fit$fit)

# Make it into a data frame for easy plotting
importance_df <- as.data.frame(model_importance)

# Plotting variable importance
importance_df$variable <- rownames(importance_df)

ggplot(importance_df, aes(x = reorder(variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Importance") +
  theme_minimal()

# What is Feature Importance?
#   Feature importance measures the relative contribution of each feature to the prediction power of the model. It is a way to understand which features are contributing the most to the model’s decisions.
# 
# How is it Calculated?
#   In Random Forest, feature importance is typically calculated in one of two ways:
#   
#   Mean Decrease in Impurity (MDI): During the construction of the Random Forest, each feature contributes to the purity of the nodes it is used in. Purity is often measured by Gini impurity or entropy for classification, and variance for regression. A feature's importance in this case is the sum of the decrease in impurity it provides weighted by the proportion of samples that reached the nodes where the feature was used.
# 
# Mean Decrease in Accuracy (MDA): Also known as permutation importance, this involves shuffling the values of each feature one by one and measuring the decrease in the model's accuracy. A larger decrease indicates a more important feature.

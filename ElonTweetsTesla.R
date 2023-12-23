
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
install.packages("tidyquant")

library(dplyr)
library(tidyverse)
library(lubridate)
library(tidyquant)

library(tidymodels)
library(textrecipes)
library(randomForest)

library(broom)
library(caret)


library(pROC)

library(ggplot2)


csv_files <- list.files(path="tweets",pattern= ".csv", full.names=TRUE)
csv_files
needed_parameters <- c("id", "date", "tweet", "hashtags", "cashtags", "username", "link", "retweet", "nlikes", "nreplies", "nretweets", "retweet_id", "retweet_date")
needed_parameters
tweet_data <- list()
for (file in csv_files) {
  #print(file) exit)=
  
  df <- read.csv(file)
  df_cleaned <- df[, colnames(df) %in% needed_parameters]
  #print(df_cleaned)
  tweet_data[[file]] <- df_cleaned
}


tesla <- tq_get('TSLA',
                from='2010-06-29',
                to='2022-03-05',
                get = "stock.prices")
head(tesla)
tesla_df <- tesla
tesla$stockpricemove <- ((tesla$close-tesla$open)/tesla$open)*100
head(tesla)
sp500_data <- tq_get('^GSPC', from ='2010-06-29', to = '2022-03-05')
head(sp500_data)
sp500_data
sp500_data$indexmove <- ((sp500_data$close-sp500_data$open)/sp500_data$open)*100
tandi <- merge.data.frame(tesla,sp500_data,by="date")
tandi$diffteslaindex <- tandi$stockpricemove - tandi$indexmove
teslaindex <- tandi[,c("date","symbol.x","open.x","close.x","volume.x","stockpricemove"
                       ,"symbol.y","open.y","close.y","volume.y","indexmove","diffteslaindex"
)]
colnames(teslaindex) <- c("date"
                          ,"stock"
                          ,"tesla_open"
                          ,"tesla_close"
                          ,"tesla_volume"
                          ,"tesla_stock_move"
                          ,"index"
                          ,"index_open"
                          ,"index_close"
                          ,"index_volume"
                          ,"index_move"
                          ,"tesla_index_diff"
)
teslaindex
file_path = "tesla/tesla_index20102022.csv"
write.csv(teslaindex,file=file_path,row.names = FALSE)

#
tweet_data <- bind_rows(tweet_data)
tweet_data <- tweet_data %>% distinct(link, .keep_all = TRUE)


stock_data <- teslaindex

stock_data$date <- as.Date(stock_data$date)
tweet_data$date <- as.Date(tweet_data$date)

tweet_data <- tweet_data %>% filter(date >= as.Date("2015-01-01") & date < as.Date("2022-01-01"))
stock_data <- stock_data %>% filter(date >= as.Date("2015-01-01") & date < as.Date("2022-01-01"))


start_date <- min(tweet_data$date)
end_date <- max(tweet_data$date)
start_date
end_date

new_dates <- seq(start_date,end_date, by="days")
add_date_df <- data.frame(date=new_dates)
add_date_df
result_tweet_data = merge(add_date_df,tweet_data,all=TRUE)
result_tweet_data
tweet_data <- result_tweet_data
merged_data <- merge(tweet_data, stock_data, by = "date")

data <- merged_data

colnames(data)

ggplot(data, aes(x = tesla_volume, y = tesla_stock_move)) + 
  geom_point() +
  labs(title = "Tesla Stock Movement vs Volume",
       x = "Volume",
       y = "Stock Movement") +
  theme_minimal()

long_data <- data %>%
  select(date, nlikes, nreplies, nretweets, tesla_stock_move, tesla_index_diff) %>%
  pivot_longer(cols = c(nlikes, nreplies, nretweets), names_to = "metric", values_to = "count")

# Creating the scatter plot
ggplot(long_data, aes(x = count, y = tesla_stock_move, color = metric)) +
  geom_point() +
  facet_wrap(~metric, scales = "free_x") +
  labs(title = "Tesla Stock Movement vs Engagement Metrics",
       x = "Count",
       y = "Tesla Stock Movement") +
  theme_minimal() +
  theme(legend.position = "bottom")

# If you want to plot index_move on the same graph but different points
ggplot(long_data, aes(x = count, y = tesla_index_diff, color = metric)) +
  geom_point() +
  facet_wrap(~metric, scales = "free_x") +
  labs(title = "Difference between index and TSLA vs Engagement Metrics",
       x = "Count",
       y = "Index Movement") +
  theme_minimal() +
  theme(legend.position = "bottom")


#correlation between metrics and TSLA stock price movement
cor(data$nlikes, data$tesla_stock_move, use = "complete.obs")
cor(data$nreplies, data$tesla_stock_move, use = "complete.obs")
cor(data$nretweets, data$tesla_stock_move, use = "complete.obs")

######################### LOGISTIC REGRESSION #############################

view(data)

data$movement_binary <- ifelse(data$tesla_index_diff > 0, 1, 0)

# Select features
features <- data %>%
  select(nlikes, nreplies, nretweets, movement_binary)

# Split data into training and test sets
set.seed(123) # For reproducibility


# Create the partition
training_indices <- createDataPartition(features$movement_binary, p = 0.8, list = FALSE)

# Split the data
train_data <- features[training_indices, ]
test_data <- features[-training_indices, ]

head(test_data)
head(train_data)

# Fit the logistic regression model
logit_model <- glm(movement_binary ~ ., data = train_data, family = binomial)

# Summary of the model to see coefficients and significance
summary(logit_model)

# After predicting probabilities...
test_data$predicted_class <- predict(logit_model, newdata = test_data, type = "response")

# Convert probabilities to a binary outcome based on a threshold
test_data$predicted_class_verbal <- ifelse(test_data$predicted_class > 0.5, "Increase", "Decrease")
test_data$predicted_class_verbal <- factor(test_data$predicted_class_verbal, levels = c("Decrease", "Increase"))

# Ensure the actual binary outcome is also a factor with the same levels
test_data$movement_binary_verbal <- ifelse(test_data$movement_binary > 0, "Increase", "Decrease")
test_data$movement_binary_verbal <- factor(test_data$movement_binary_verbal, levels = c("Decrease", "Increase"))

# ROC curve calculation using actual binary outcome and predicted probabilities
roc_curve <- roc(as.numeric(test_data$movement_binary_verbal) - 1, test_data$predicted_class)
plot(roc_curve)

# Confusion matrix using verbal class predictions and actual binary outcome
conf_matrix <- confusionMatrix(test_data$predicted_class_verbal, test_data$movement_binary_verbal)
print(conf_matrix)

# Sensitivity (also called the true positive rate or recall) is quite high at 0.8613, meaning the model is good at identifying the positive class.
# Specificity is very low at 0.2000, indicating that the model is not good at identifying the negative class.
# Positive Predictive Value (PPV) and Negative Predictive Value (NPV) are around 0.54 and 0.56, respectively, which are not much better than a random guess.
# Balanced Accuracy, which is the average of sensitivity and specificity, is 0.5307, just slightly better than a random guess, suggesting that the model's ability to classify both positive and negative classes is not impressive.

######################### LOGISTIC REGRESSION #############################



###################### LASSO REGRESSION #####################


library(glmnet)

# Select features
features <- data %>%
  select(nlikes, nreplies, nretweets, movement_binary)

features$nlikes <- ifelse(is.na(features$nlikes), 0, features$nlikes)
features$nreplies <- ifelse(is.na(features$nreplies), 0, features$nreplies)
features$nretweets <- ifelse(is.na(features$nretweets), 0, features$nretweets)
sum(is.na(features))

# Split data into training and test sets
set.seed(123) # For reproducibility

# Create the partition
training_indices <- createDataPartition(features$movement_binary, p = 0.8, list = FALSE)

# Split the data
train_data_lasso <- features[training_indices, ]
test_data_lasso <- features[-training_indices, ]

# Ensure that the number of rows in the training data matches the length of training indices
stopifnot(nrow(train_data_lasso) == length(training_indices))

# Prepare the matrix of predictors
x <- model.matrix(movement_binary ~ . - 1, data = train_data_lasso) # -1 to exclude intercept

y <- train_data_lasso$movement_binary

# Ensure that the number of observations in y matches the number of rows in x
sum(is.na(y))

# Fit the Lasso model
cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
coef(lasso_model)
plot(cv_lasso)

# Find the best lambda (regularization parameter)
best_lambda <- cv_lasso$lambda.min

# Now fit the model using the best lambda
#Try different alpha(penalty) values
lasso_model <- glmnet(x, y, alpha = 0.1, lambda = best_lambda, family = "binomial")
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
x_test <- model.matrix(~ ., test_data_lasso)


predictors_in_model <- rownames(lasso_model$beta) %>%
  na.omit() %>%
  as.character()
x_test <- x_test[, predictors_in_model, drop = FALSE]


predictions <- predict(lasso_model, newx = x_test, type = "response")

head(test_data_lasso)

# Convert probabilities to a binary outcome based on a threshold
test_data_lasso$predicted_class_verbal <- ifelse(predictions > 0.5, "Increase", "Decrease")
test_data_lasso$predicted_class_verbal <- factor(test_data_lasso$predicted_class_verbal, levels = c("Decrease", "Increase"))

# Assuming price_movement_binary_verbal is your actual outcome column and has been properly created:
test_data_lasso$movement_binary_verbal <- ifelse(test_data_lasso$movement_binary > 0, "Increase", "Decrease")
test_data_lasso$movement_binary_verbal <- factor(test_data_lasso$movement_binary_verbal, levels = c("Decrease", "Increase"))

# Now calculate the confusion matrix
conf_matrix <- confusionMatrix(test_data_lasso$predicted_class_verbal, test_data_lasso$movement_binary_verbal)
print(conf_matrix)

# Optionally, plot the coefficients
plot(lasso_model, xvar = "lambda", label = TRUE)

###################### LASSO REGRESSION #####################
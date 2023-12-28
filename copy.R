
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("stringr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")


library(dplyr)
library(tidyverse)
library(lubridate)


csv_files <- list.files(path="Elon-Tesla-Relationship/tweets",pattern= ".csv", full.names=TRUE)
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

stock_data <- read.csv("Elon-Tesla-Relationship/tesla/tesla20102022.csv")


stock_data$date <- as.Date(stock_data$date)
tweet_data$date <- as.Date(tweet_data$date)
tweet_data <- tweet_data %>% filter(date >= as.Date("2015-01-01") & date < as.Date("2022-01-01"))
stock_data <- stock_data %>% filter(date >= as.Date("2015-01-01") & date < as.Date("2022-01-01"))


head(merged_df)
head(stock_data)
merged_data <- merge(tweet_data, stock_data, by = "date")
head(merged_data)

max(tweet_data$date)
max(stock_data$date)

merged_data$price_movement <- ((merged_data$close - merged_data$open) / merged_data$open) * 100

daily_summary <- merged_data %>%
  group_by(date) %>%
  summarise(total_likes = sum(nlikes), 
            price_movement = mean(price_movement))

# Plot
ggplot(daily_summary, aes(x = total_likes, y = price_movement)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tweet Likes vs Stock Price Movement",
       x = "Total Likes",
       y = "Price Movement")

#correlation between sum of tweet likes in a day and TSLA stock price movement
cor(daily_summary$total_likes, daily_summary$price_movement, use = "complete.obs")

library(stringr)
library(rpart)
library(rpart.plot)
library(caret)

# Assuming tweet_data and stock_data are already loaded and merged into a data frame called 'data'

# Create a binary target variable for stock movement: 1 for increase, 0 for decrease/no change
data$Stock_Move <- ifelse(daily_summary$price_movement > 0, 1, 0)

# Function to count hashtags and cashtags
count_hashtags <- function(tweet) {
  length(str_extract_all(tweet, "#\\S+")[[1]])
}

count_cashtags <- function(tweet) {
  length(str_extract_all(tweet, "\\$[A-Z]+")[[1]])
}

# Apply the functions to the tweet column
data$hashtag_count <- sapply(data$tweet, count_hashtags)
data$cashtag_count <- sapply(data$tweet, count_cashtags)

# Create 14-day lagged features for 'nlikes', 'hashtag_count', and 'cashtag_count'
lag_features <- c("nlikes", "hashtag_count", "cashtag_count")
for (feature in lag_features) {
  for (i in 1:14) {
    lagged_col_name <- paste(feature, "lag", i, sep = "_")
    data[[lagged_col_name]] <- stats::lag(data[[feature]], -i)
  }
}

# Remove NA values created due to lagging
data <- na.omit(data)

# Prepare the dataset for the decision tree
features <- data %>%
  select(contains("lag"), Stock_Move)  # select all lagged features and Stock Move

# Split data into training and test sets
set.seed(123) # For reproducibility
index <- createDataPartition(features$Stock_Move, p = 0.8, list = FALSE)
train_data <- features[index, ]
test_data <- features[-index, ]

# Train the decision tree model
dt_model <- rpart(Stock_Move ~ ., data = train_data, method = "class")

# Plot the decision tree
rpart.plot(dt_model)

# Predict on test set
predictions <- predict(dt_model, test_data, type = "class")

# Evaluate the model using a confusion matrix
confusionMatrix(predictions, test_data$Stock_Move)

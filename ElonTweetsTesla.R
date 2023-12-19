
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
library(dplyr)
library(tidyverse)
library(lubridate)
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
stock_data <- read.csv("tesla/tesla20102022.csv")


stock_data$date <- as.Date(stock_data$date)
tweet_data$date <- as.Date(tweet_data$date)
head(merged_df)
head(stock_data)
merged_data <- merge(tweet_data, stock_data, by = "date")
head(merged_data)

merged_data$price_movement <- merged_data$close - merged_data$open

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



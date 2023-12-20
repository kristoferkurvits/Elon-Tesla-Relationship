
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
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
stock_data <- read.csv("Elon-Tesla-Relationship/tesla/tesla20102022.csv")


stock_data$date <- as.Date(stock_data$date)
tweet_data$date <- as.Date(tweet_data$date)
tweet_data <- tweet_data %>% filter(date >= as.Date("2015-01-01") & date <= as.Date("2022-01-01"))
stock_data <- stock_data %>% filter(date >= as.Date("2015-01-01") & date <= as.Date("2022-01-01"))


head(merged_df)
head(stock_data)
merged_data <- merge(tweet_data, stock_data, by = "date")
head(merged_data)

max(tweet_data$date)
max(stock_data$date)

merged_data$price_movement <- merged_data$close - merged_data$open

daily_summary <- merged_data %>%
  group_by(date) %>%
  summarise(total_likes = sum(nlikes), 
            price_movement = mean(price_movement))

# Plot
ggplot(daily_summary, aes(x = date)) +
  geom_line(aes(y = total_likes, colour = "Total Likes")) +
  geom_line(aes(y = price_movement, colour = "Price Movement")) +
  labs(title = "Daily Total Likes and Price Movement Over Time",
       x = "Date",
       y = "Value") +
  scale_colour_manual("", 
                      breaks = c("Total Likes", "Price Movement"),
                      values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

#correlation between sum of tweet likes in a day and TSLA stock price movement
cor(daily_summary$total_likes, daily_summary$price_movement, use = "complete.obs")



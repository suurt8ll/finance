library(dplyr)
library(tidyr)

# Load data
n225 <- read.csv("N225.csv") %>% select(Date, Close)
usdjpy <- read.csv("USDJPY_long_term.csv") %>% select(Date, Close)

# Format data
n225$Date <- as.Date(n225$Date)
usdjpy$Date <- as.Date(usdjpy$Date)

# Merge the dataframes by Date, keeping all rows from n225 and fill missing values in usdjpy
merged_data <- n225 %>%
  filter(Date >= min(usdjpy$Date)) %>%
  left_join(usdjpy, by = "Date", suffix = c("_n225", "_usdjpy")) %>%
  fill(Close_usdjpy, .direction = "updown")

# Calculate Nikkei 225 price in USD
merged_data <- merged_data %>%
  mutate(Nikkei_in_USD = Close_n225 / Close_usdjpy)

# View the result
tail(merged_data)

# Select and rename columns
final_data <- merged_data %>%
  select(Date, Close = Nikkei_in_USD)

# Save to a CSV file
write.csv(final_data, "N225USD.csv", row.names = FALSE, quote = FALSE)
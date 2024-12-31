library(tidyverse)
library(viridis)

# Load the data
bitcoin <- read_csv("./data/bitcoin_prices.csv")

# Calculate regular and log returns
bitcoin <- bitcoin %>%
  mutate(
    Lagged_Close = lag(Close),
    Regular_Return = (Close - Lagged_Close) / Lagged_Close,
    Log_Return = log(Close / Lagged_Close)
  )

# Find the 10 rows where the regular and log returns differ the most
largest_differences <- bitcoin %>%
  mutate(Difference = abs(Regular_Return - Log_Return)) %>%
  arrange(desc(Difference)) %>%
  slice_head(n = 10)

# Output the 10 rows with the largest differences
print(largest_differences)

# Find the 10 largest log return instances
largest_log_returns <- bitcoin %>%
  arrange(desc(Log_Return)) %>%
  slice_head(n = 10)

# Output the 10 largest log returns
print(largest_log_returns)

# Find the 10 lowest log return instances
lowest_log_returns <- bitcoin %>%
  arrange(Log_Return) %>%
  slice_head(n = 10)

# Output the 10 lowest log returns
print(lowest_log_returns)

# Filter data for log returns between -0.4 and 0.4
filtered_bitcoin <- bitcoin %>%
  filter(Log_Return >= -0.4 & Log_Return <= 0.4)

# Count the number of data points excluded
excluded_count <- nrow(bitcoin) - nrow(filtered_bitcoin)
cat("Number of excluded data points:", excluded_count, "\n")

# Visualize the distribution of regular returns
filtered_bitcoin %>%
  ggplot(aes(x = Regular_Return)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.01, fill = viridis(1, option = "D"), color = NA, alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(filtered_bitcoin$Regular_Return, na.rm = TRUE), sd = sd(filtered_bitcoin$Regular_Return, na.rm = TRUE)), color = viridis(1, option = "C"), size = 1) +
  labs(
    title = "Distribution of Regular Returns",
    x = "Regular Return",
    y = "Density"
  ) +
  theme_minimal()

# Visualize the distribution of log returns
filtered_bitcoin %>%
  ggplot(aes(x = Log_Return)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.01, fill = viridis(1, option = "D"), color = NA, alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(filtered_bitcoin$Log_Return, na.rm = TRUE), sd = sd(filtered_bitcoin$Log_Return, na.rm = TRUE)), color = viridis(1, option = "C"), size = 1) +
  labs(
    title = "Distribution of Log Returns",
    x = "Log Return",
    y = "Density"
  ) +
  theme_minimal()

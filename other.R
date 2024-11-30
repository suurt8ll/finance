# ---- Load Necessary Libraries ----
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# ---- Load and Prepare Data ----
# Load the data from the CSV file
csv_file <- "bitcoin_daily_prices.csv"
#csv_file <- "Tartu.csv"
price_data <- read.csv(csv_file)

# Ensure your Date column is in Date format
price_data$Date <- as.Date(price_data$Date)

# Sort data by Date
price_data <- price_data[order(price_data$Date), ]


# ---- Determine Full Year Range ----
# Find the earliest and latest dates
min_date <- min(price_data$Date)
max_date <- max(price_data$Date)

# Determine the earliest full year
start_year <- ifelse(month(min_date) == 1 & day(min_date) == 1, year(min_date), year(min_date) + 1)
end_year <- year(max_date)

# Adjust the start date to January 1st of the earliest full year
start_date <- as.Date(paste0(start_year, "-01-01"))
end_date <- max_date

# ---- Calculate CAGR for Overlapping Periods ----
# Create a dataframe to store CAGR results
cagr_results_yearly <- data.frame(Period = character(),
                                  CAGR = numeric(),
                                  stringsAsFactors = FALSE)

# Function to calculate CAGR
calculate_cagr <- function(start_date, end_date, data) {
  period_data <- data[data$Date >= start_date & data$Date <= end_date, ]
  if (nrow(period_data) > 0) {
    start_value <- period_data$Close[1]
    end_value <- period_data$Close[nrow(period_data)]
    period_years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
    cagr <- (end_value / start_value)^(1 / period_years) - 1
    return(cagr)
  } else {
    return(NA)
  }
}

# Loop through each possible start year and calculate CAGR
for (start_yr in start_year:end_year) {
  start_date <- as.Date(paste0(start_yr, "-01-01"))
  cagr <- calculate_cagr(start_date, end_date, price_data)
  period_label <- paste0(start_yr, "-", end_year)
  cagr_results_yearly <- rbind(cagr_results_yearly, data.frame(Period = period_label, CAGR = cagr))
}

# ---- Visualize Overlapping Period CAGR ----
# Ensure the Period column is ordered chronologically
cagr_results_yearly$Period <- factor(cagr_results_yearly$Period, levels = rev(unique(cagr_results_yearly$Period)))

# Create a bar plot for CAGR
ggplot(cagr_results_yearly, aes(x = Period, y = CAGR)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(
    title = "Compound Annual Growth Rate (CAGR) for Overlapping Periods",
    x = "Period",
    y = "CAGR"
  ) +
  geom_text(
    aes(label = scales::percent(CAGR, accuracy = 1)),
    hjust = -0.25,
    color = "black"
  ) +
  theme_minimal() +
  coord_flip()  # Flip the coordinates for better readability

# ---- Interactive Financial Freedom Calculator ----
financial_freedom <- function() {
  # Ask the user for inputs
  current_investment <- as.numeric(readline(prompt = "Enter your current investment size ($): "))
  withdrawal_rate <- as.numeric(readline(prompt = "Enter your withdrawal rate (e.g., 0.04 for 4%): "))
  annual_expenses <- as.numeric(readline(prompt = "Enter your annual expenses ($): "))
  cagr <- as.numeric(readline(prompt = "Enter your projected CAGR (e.g., 0.2 for 20%): "))
  
  # Calculate target portfolio size
  target_portfolio <- annual_expenses / withdrawal_rate
  
  # Calculate years to financial freedom
  years_to_freedom <- log(target_portfolio / current_investment) / log(1 + cagr)
  
  # Display results
  if (years_to_freedom > 0) {
    cat("\nTo achieve financial freedom:")
    cat("\n - Target Portfolio Size: $", round(target_portfolio, 2))
    cat("\n - Years Needed: ", round(years_to_freedom, 2), " years\n")
  } else {
    cat("\nCongratulations! You already have enough for financial freedom based on your inputs.\n")
  }
}

# Run the financial freedom calculator
financial_freedom()
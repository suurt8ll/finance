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
bitcoin_data <- read.csv("bitcoin_daily_prices.csv")

# Ensure your Date column is in Date format
bitcoin_data$Date <- as.Date(bitcoin_data$Date)

# Sort data by Date
bitcoin_data <- bitcoin_data[order(bitcoin_data$Date), ]

# ---- Determine Full Year Range ----
# Find the earliest and latest dates
min_date <- min(bitcoin_data$Date)
max_date <- max(bitcoin_data$Date)

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
  cagr <- calculate_cagr(start_date, end_date, bitcoin_data)
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

# ---- Subperiod CAGR Calculation Function ----

calculate_subperiod_cagrs <- function(data, period_length) {
  # Ensure the dataset is sorted by date
  data <- data[order(data$Date), ]
  
  # Initialize a dataframe to store subperiod CAGR results
  cagr_results <- data.frame(
    Start_Date = as.Date(character()),
    End_Date = as.Date(character()),
    CAGR = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through all possible start dates
  for (i in 1:(nrow(data) - 1)) {
    start_date <- data$Date[i]
    end_date <- start_date + as.difftime(period_length * 365.25, units = "days")
    
    # Filter for the subperiod
    subperiod_data <- data[data$Date >= start_date & data$Date <= end_date, ]
    
    # If a valid subperiod exists, calculate the CAGR
    if (nrow(subperiod_data) > 1) {
      start_value <- subperiod_data$Close[1]
      end_value <- subperiod_data$Close[nrow(subperiod_data)]
      years <- as.numeric(difftime(max(subperiod_data$Date), start_date, units = "days")) / 365.25
      cagr <- (end_value / start_value)^(1 / years) - 1
      
      # Append the result to the dataframe
      cagr_results <- rbind(
        cagr_results,
        data.frame(Start_Date = start_date, End_Date = max(subperiod_data$Date), CAGR = cagr)
      )
    }
  }
  
  return(cagr_results)
}

# ---- Subperiod CAGR Visualization Function ----

visualize_subperiod_cagrs <- function(cagr_results, period_length) {
  library(ggplot2)
  
  # Calculate IQR and determine bounds
  Q1 <- quantile(cagr_results$CAGR, 0.25)
  Q3 <- quantile(cagr_results$CAGR, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filter out outliers
  filtered_results <- cagr_results[cagr_results$CAGR >= lower_bound & cagr_results$CAGR <= upper_bound, ]
  
  # Calculate outlier stats
  total_points <- nrow(cagr_results)
  remaining_points <- nrow(filtered_results)
  outliers_removed <- total_points - remaining_points
  
  # Print information about outliers
  cat("\nOutlier Removal Summary:\n")
  cat("- Total data points: ", total_points, "\n")
  cat("- Remaining data points after outlier removal: ", remaining_points, "\n")
  cat("- Number of outliers removed: ", outliers_removed, "\n")
  cat("- Percentage of data points removed: ", round((outliers_removed / total_points) * 100, 2), "%\n")
  
  # Plot the distribution of CAGRs without outliers
  ggplot(filtered_results, aes(x = CAGR)) +
    geom_density(fill = "blue", alpha = 0.4) +
    labs(
      title = paste("CAGR Distribution for", period_length, "Year Holding Periods (No Outliers)"),
      x = "CAGR",
      y = "Density"
    ) +
    theme_minimal()
}

# ---- Subperiod CAGR Summary Function ----

summarize_subperiod_cagrs <- function(cagr_results, period_length) {
  if (nrow(cagr_results) == 0) {
    cat("\nNo valid subperiods found for the specified holding period length.\n")
    return(NULL)
  }
  
  # Summary Statistics
  mean_cagr <- mean(cagr_results$CAGR)
  median_cagr <- median(cagr_results$CAGR)
  min_cagr <- min(cagr_results$CAGR)
  max_cagr <- max(cagr_results$CAGR)
  deciles <- quantile(cagr_results$CAGR, probs = seq(0, 1, by = 0.1))
  
  # Display textual summary
  cat("\nCAGR Summary Statistics for", period_length, "Year Holding Periods:\n")
  cat("- Mean CAGR: ", scales::percent(mean_cagr, accuracy = 0.1), "\n")
  cat("- Median CAGR: ", scales::percent(median_cagr, accuracy = 0.1), "\n")
  cat("- Minimum CAGR: ", scales::percent(min_cagr, accuracy = 0.1), "\n")
  cat("- Maximum CAGR: ", scales::percent(max_cagr, accuracy = 0.1), "\n")
  cat("\nDecile Analysis:\n")
  for (i in 1:length(deciles)) {
    cat(paste0(" - ", names(deciles)[i], ": ", scales::percent(deciles[i], accuracy = 0.1)), "\n")
  }
}

# ---- Call the Functions ----

# Specify the period length
#period_length <- as.numeric(readline(prompt = "Enter the holding period length (in years): "))
period_length <- 8

# Step 1: Calculate the subperiod CAGRs
cagr_results <- calculate_subperiod_cagrs(bitcoin_data, period_length)

# Step 2: Visualize the subperiod CAGRs
if (!is.null(cagr_results) && nrow(cagr_results) > 0) {
  visualize_subperiod_cagrs(cagr_results, period_length)
}

# Step 3: Summarize the subperiod CAGRs
summarize_subperiod_cagrs(cagr_results, period_length)

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
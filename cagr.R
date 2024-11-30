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


# ---- Functions ----

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
    end_date <- start_date %m+% years(period_length) # Add precise years
    
    # Skip if the calculated end date exceeds the maximum date in the data
    if (end_date > max(data$Date)) {
      next
    }
    
    subperiod_data <- data[data$Date >= start_date & data$Date <= end_date, ]
    
    if (nrow(subperiod_data) > 1) {
      actual_years <- as.numeric(interval(start_date, max(subperiod_data$Date)) / years(1))
      
      start_value <- subperiod_data$Close[1]
      end_value <- subperiod_data$Close[nrow(subperiod_data)]
      cagr <- (end_value / start_value)^(1 / actual_years) - 1
      
      cagr_results <- rbind(
        cagr_results,
        data.frame(Start_Date = start_date, End_Date = max(subperiod_data$Date), CAGR = cagr)
      )
    }
  }
  
  return(cagr_results)
}

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
period_length <- 4

# Step 1: Calculate the subperiod CAGRs
cagr_results <- calculate_subperiod_cagrs(price_data, period_length)

# Step 2: Visualize the subperiod CAGRs
if (!is.null(cagr_results) && nrow(cagr_results) > 0) {
  visualize_subperiod_cagrs(cagr_results, period_length)
}

# Step 3: Summarize the subperiod CAGRs
summarize_subperiod_cagrs(cagr_results, period_length)
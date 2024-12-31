library(tidyverse)
library(lubridate)
library(viridis)
library(zoo)
library(plotly)

# Function to calculate and plot rolling volatility for multiple dataframes
# - `assets`: A tibble containing asset details: Name, FileName, Traded247.
# - `increment_days`: The number of days in each increment (e.g., 1 for daily, 7 for weekly).
# - `period`: The rolling window size, measured in increments (not days).
# - `log_returns`: Whether to calculate logged returns or simple returns.
calculate_volatility_comparison <- function(assets, increment_days = 7, period = 12, log_returns = TRUE) {
  # Load and process each file
  data_list <- map(assets$FileName, ~ read_csv(file.path("./data", .x)))
  names(data_list) <- assets$Name
  
  # Process each dataframe in the list
  processed_data <- map_dfr(assets$Name, function(name) {
    data <- data_list[[name]] %>% arrange(Date)
    
    # Determine the annualization factor dynamically for this instrument
    annualization_factor <- if (assets$Traded247[assets$Name == name]) {
      sqrt(365.25 / increment_days)
    } else {
      sqrt(252 / increment_days)  # Typical workdays in a year
    }
    
    if (increment_days > 1) {
      data <- data %>% 
        mutate(Increment = floor_date(Date, unit = paste(increment_days, "days"))) %>% 
        group_by(Increment) %>% 
        summarize(Close = last(Close)) %>% 
        ungroup()
      data <- data %>% rename(Date = Increment)
    }
    
    # Calculate returns
    if (log_returns) {
      data <- data %>% mutate(Returns = log(Close / lag(Close)))
    } else {
      data <- data %>% mutate(Returns = (Close / lag(Close)) - 1)
    }
    
    # Calculate rolling standard deviation
    data <- data %>% 
      mutate(Rolling_SD = rollapply(Returns, width = period, FUN = sd, fill = NA, align = "right"))
    
    # Annualize volatility
    data <- data %>% 
      mutate(Annualized_Volatility = Rolling_SD * annualization_factor)
    
    # Add a column for the instrument name
    data <- data %>% mutate(Instrument = name)
    
    return(data)
  })
  
  # Plot the annualized volatility
  plot <- processed_data %>% 
    ggplot(aes(x = Date, y = Annualized_Volatility, color = Instrument)) +
    geom_line() +
    scale_color_viridis_d() +
    labs(
      title = "Annualized Volatility Comparison",
      x = "Date",
      y = "Annualized Volatility",
      color = "Instrument"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal(base_size = 14)
  
  # Convert ggplot to interactive plotly and add a subtitle
  interactive_plot <- ggplotly(plot) %>%
    layout(
      title = list(
        text = paste0(
          "Annualized Volatility Comparison",
          "<br>",
          "<sup>",
          "Increment: ", increment_days, " days, ",
          "Rolling Period: ", period, ", ",
          if (log_returns) "Log Returns" else "Simple Returns",
          "</sup>"
        )
      )
    )
  
  return(list(data = processed_data, plot = interactive_plot))
}

# Define assets
assets <- tibble::tibble(
  Name = c("Bitcoin", "Gold", "Natural Gas"),
  FileName = c("bitcoin_prices.csv", "gold_prices.csv", "gas_prices.csv"),
  Traded247 = c(TRUE, FALSE, FALSE)
)

# Example usage
result <- calculate_volatility_comparison(
  assets = assets,
  increment_days = 7, 
  period = 52, 
  log_returns = TRUE
)
result$plot

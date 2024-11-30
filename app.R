library(shiny)
library(ggplot2)
library(lubridate)

# Function to calculate subperiod CAGRs
calculate_subperiod_cagrs <- function(data, period_length, selected_range) {
  data <- data[order(data$Date), ]
  cagr_results <- data.frame(
    Start_Date = as.Date(character()),
    End_Date = as.Date(character()),
    CAGR = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Extract the start and end of the selected range
  selected_start <- selected_range[1]
  selected_end <- selected_range[2]
  
  # Use withProgress to track progress
  withProgress(message = "Calculating CAGRs", value = 0, {
    for (i in 1:(nrow(data) - 1)) {
      start_date <- data$Date[i]
      end_date <- start_date %m+% years(period_length)
      
      # Skip if start_date or end_date is outside the selected range
      if (start_date < selected_start || end_date > selected_end) {
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
      
      # Increment progress
      incProgress(1 / (nrow(data) - 1))
    }
  })
  
  return(cagr_results)
}

# Visualization function
visualize_subperiod_cagrs <- function(cagr_results, period_length) {
  Q1 <- quantile(cagr_results$CAGR, 0.25)
  Q3 <- quantile(cagr_results$CAGR, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  filtered_results <- cagr_results[cagr_results$CAGR >= lower_bound & cagr_results$CAGR <= upper_bound, ]
  
  outliers_removed <- nrow(cagr_results) - nrow(filtered_results)
  percentage_removed <- (outliers_removed / nrow(cagr_results)) * 100
  
  ggplot(filtered_results, aes(x = CAGR)) +
    geom_density(fill = "blue", alpha = 0.4) +
    labs(
      title = paste("CAGR Distribution for", period_length, "Year Holding Periods"),
      subtitle = paste(outliers_removed, "outliers removed (", round(percentage_removed, 2), "% of total)", sep = ""),
      x = "CAGR",
      y = "Density"
    ) +
    theme_minimal()
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Bitcoin CAGR Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("file_select", "Select CSV File:", choices = list.files(pattern = "\\.csv$"), selected = NULL),
      fileInput("file_upload", "Or Upload a New CSV File:", accept = ".csv"),
      dateRangeInput(
        "date_range", 
        "Select Date Range:", 
        start = NULL, 
        end = NULL, 
        min = NULL, 
        max = NULL
      ),
      numericInput("period", "Holding Period (Years):", value = 8, min = 1, step = 1),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Visualization", plotOutput("plot"))
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Reactive value to store the temporary cache
  cache <- reactiveValues()
  
  # Load data from the selected or uploaded file
  data <- reactive({
    req(input$file_select)
    file_path <- file.path(getwd(), input$file_select)
    df <- read.csv(file_path)
    df$Date <- as.Date(df$Date)
    df
  })
  
  # Update the date range input based on the dataset
  observe({
    req(data())
    updateDateRangeInput(
      session, 
      "date_range", 
      start = min(data()$Date), 
      end = max(data()$Date), 
      min = min(data()$Date), 
      max = max(data()$Date)
    )
  })
  
  # Filter data based on the selected date range
  filtered_data <- reactive({
    req(data(), input$date_range)
    data()[data()$Date >= input$date_range[1] & data()$Date <= input$date_range[2], ]
  })
  
  # Calculate results with temporary caching
  results <- eventReactive(input$analyze, {
    req(filtered_data())
    period_length <- input$period
    file_name <- input$file_select
    date_range_key <- paste(as.character(input$date_range), collapse = "_")
    
    # Create a unique key for the cache
    cache_key <- paste(file_name, period_length, date_range_key, sep = "_")
    
    # Check if the result is already cached
    if (!is.null(cache[[cache_key]])) {
      message("Using cached results for ", cache_key)
      return(cache[[cache_key]])
    }
    
    # Otherwise, compute and store the results in the cache
    result <- calculate_subperiod_cagrs(filtered_data(), period_length, input$date_range)
    cache[[cache_key]] <- result
    return(result)
  })
  
  # Render summary
  output$summary <- renderPrint({
    req(results())
    if (nrow(results()) == 0) {
      cat("No valid subperiods found for the specified holding period length.\n")
    } else {
      mean_cagr <- mean(results()$CAGR)
      median_cagr <- median(results()$CAGR)
      min_cagr <- min(results()$CAGR)
      max_cagr <- max(results()$CAGR)
      deciles <- quantile(results()$CAGR, probs = seq(0, 1, by = 0.1))
      cat("CAGR Summary Statistics:\n")
      cat("- Mean CAGR: ", scales::percent(mean_cagr, accuracy = 0.1), "\n")
      cat("- Median CAGR: ", scales::percent(median_cagr, accuracy = 0.1), "\n")
      cat("- Minimum CAGR: ", scales::percent(min_cagr, accuracy = 0.1), "\n")
      cat("- Maximum CAGR: ", scales::percent(max_cagr, accuracy = 0.1), "\n")
      cat("\nDecile Analysis:\n")
      for (i in 1:length(deciles)) {
        cat(paste0(" - ", names(deciles)[i], ": ", scales::percent(deciles[i], accuracy = 0.1)), "\n")
      }
    }
  })
  
  # Render plot
  output$plot <- renderPlot({
    req(results())
    visualize_subperiod_cagrs(results(), input$period)
  })
}

# Run the app
shinyApp(ui, server)
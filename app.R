library(shiny)
library(ggplot2)
library(lubridate)

# Functions (as before)
calculate_subperiod_cagrs <- function(data, period_length) {
  data <- data[order(data$Date), ]
  cagr_results <- data.frame(
    Start_Date = as.Date(character()),
    End_Date = as.Date(character()),
    CAGR = numeric(),
    stringsAsFactors = FALSE
  )
  for (i in 1:(nrow(data) - 1)) {
    start_date <- data$Date[i]
    end_date <- start_date %m+% years(period_length)
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
  Q1 <- quantile(cagr_results$CAGR, 0.25)
  Q3 <- quantile(cagr_results$CAGR, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  filtered_results <- cagr_results[cagr_results$CAGR >= lower_bound & cagr_results$CAGR <= upper_bound, ]
  
  # Calculate the number and percentage of outliers removed
  total_points <- nrow(cagr_results)
  outliers_removed <- total_points - nrow(filtered_results)
  percentage_removed <- (outliers_removed / total_points) * 100
  
  # Plot the distribution of CAGRs without outliers
  ggplot(filtered_results, aes(x = CAGR)) +
    geom_density(fill = "blue", alpha = 0.4) +
    labs(
      title = paste("CAGR Distribution for", period_length, "Year Holding Periods (No Outliers)"),
      subtitle = paste(outliers_removed, "outliers removed (", 
                       round(percentage_removed, 2), "% of total)", sep = ""),
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
  # Reactive value to track selected or uploaded file
  current_file <- reactiveVal(NULL)
  
  # Update file dropdown when a new file is uploaded
  observeEvent(input$file_upload, {
    req(input$file_upload)
    file_path <- file.path(getwd(), input$file_upload$name)
    file.copy(input$file_upload$datapath, file_path, overwrite = TRUE)
    updateSelectInput(session, "file_select", choices = list.files(pattern = "\\.csv$"), selected = input$file_upload$name)
    current_file(file_path)
  })
  
  # Update current file when a file is selected from the dropdown
  observeEvent(input$file_select, {
    req(input$file_select)
    current_file(file.path(getwd(), input$file_select))
  })
  
  # Load data from the selected or uploaded file
  data <- reactive({
    req(current_file())
    file <- current_file()
    df <- read.csv(file)
    df$Date <- as.Date(df$Date)
    df
  })
  
  # Calculate results when the "Analyze" button is clicked
  results <- eventReactive(input$analyze, {
    req(data())
    calculate_subperiod_cagrs(data(), input$period)
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
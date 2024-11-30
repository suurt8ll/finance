library(shiny)
library(ggplot2)
library(lubridate)

# Functions (from your provided script)
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
    end_date <- start_date %m+% years(period_length) # Add precise years
    
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
  ggplot(filtered_results, aes(x = CAGR)) +
    geom_density(fill = "blue", alpha = 0.4) +
    labs(
      title = paste("CAGR Distribution for", period_length, "Year Holding Periods (No Outliers)"),
      x = "CAGR",
      y = "Density"
    ) +
    theme_minimal()
}

# Shiny UI
ui <- fluidPage(
  titlePanel("CAGR Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
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
  data <- reactive({
    req(input$file)
    file <- input$file$datapath
    df <- read.csv(file)
    df$Date <- as.Date(df$Date)
    df
  })
  
  results <- eventReactive(input$analyze, {
    req(data())
    calculate_subperiod_cagrs(data(), input$period)
  })
  
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
  
  output$plot <- renderPlot({
    req(results())
    visualize_subperiod_cagrs(results(), input$period)
  })
}

# Run the app
shinyApp(ui, server)
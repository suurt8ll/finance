# Load necessary libraries
library(ggplot2)
library(plotly)
library(reshape2)
library(lubridate) # For handling dates

# Read the data, excluding Listings columns
data <- read.csv(
  "./data/extracted_data.csv",
  colClasses = c(
    "character", # Date
    "numeric",   # Price_A
    "NULL",      # Listings_A (excluded)
    "integer",   # Active_A
    "numeric",   # Price_B
    "NULL",      # Listings_B (excluded)
    "integer"    # Active_B
  )
)

# Rename columns for easier use
colnames(data) <- c("Date", "Price_A", "Active_A", "Price_B", "Active_B")

# If you want the last day of the month instead:
data$Date <- as.Date(paste0("01.", data$Date), format = "%d.%m.%Y") # Parse as first day
data$Date <- ceiling_date(data$Date, "month") - days(1) # Shift to last day of the month

# Reshape data for faceting
long_data <- melt(data, id.vars = "Date", variable.name = "Metric", value.name = "Value")

# Add a facet category
long_data$Facet <- ifelse(long_data$Metric %in% c("Price_A", "Price_B"), "Prices (€/m², Log Scale)", "Active Listings (Log Scale)")
# Modify the Facet column levels and include descriptive y-axis labels
long_data$Facet <- factor(long_data$Facet, levels = c("Prices (€/m², Log Scale)", "Active Listings (Log Scale)"))

# Update the scale_y_log10 for both facets
gg <- ggplot(long_data, aes(x = Date, y = Value, color = Metric)) +
  geom_line(aes(y = Value, color = Metric), size = 1) +
  scale_y_log10(labels = scales::comma, name = NULL) +  # Log scale for both facets
  facet_grid(rows = vars(Facet),
             scales = "free_y",
             switch = "y") +  # Separate panels
  scale_color_manual(
    values = c(
      "Price_A" = "blue",
      "Price_B" = "red",
      "Active_A" = "lightblue",
      "Active_B" = "pink"
    ),
    labels = c("Price A", "Price B", "Active Listings A", "Active Listings B")
  ) +
  labs(title = "Tartu Real Estate Price Trends and Active Listings", x = "Date", color = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.placement = "outside",
    # Place facet labels outside
    strip.background = element_blank(),
    legend.position = "bottom"
  )
ggplotly(gg)

# Prepare the new data frame with Date and Close columns
new_data <- data.frame(Date = data$Date, Close = data$Price_A)
# Save the new data frame to a CSV file
write.csv(new_data, "./data/Tartu.csv", row.names = FALSE, quote = FALSE)
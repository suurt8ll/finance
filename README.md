# CAGR Analyzer

This is an interactive Shiny application to analyze provided financial asset's Compound Annual Growth Rate (CAGR) over specified subperiods. It provides insights into subperiod performance, visualizations of CAGR distributions, and price history.

---

## Features

- **Subperiod CAGR Calculation:** Analyze CAGR for custom holding periods within a selected date range.
- **Visualization:** Density plot of CAGRs with outlier filtering and price history with a log scale.
- **Interactive Interface:** Select CSV files, set date ranges, and specify holding periods via a user-friendly UI.

---

## Requirements

- **Linux Distribution:** Tested on Debian-based (e.g., Ubuntu) and Arch-based systems.
- **R:** The application requires R and the Shiny package.

---

## Installation and Usage

### Automated Setup (Recommended)

1. Run the following command to set up everything from scratch:
   ```bash
   curl -sL https://github.com/suurt8ll/cagr-analyzer/new/master/setup.sh | bash
   ```

2. Access the Shiny app in your browser at `http://localhost:3838`.

---

### Manual Setup

1. Install R:
   - For Debian-based systems:
     ```bash
     sudo apt update && sudo apt install -y r-base
     ```
   - For Arch-based systems:
     ```bash
     sudo pacman -Syu r
     ```

2. Clone the repository:
   ```bash
   git clone https://github.com/suurt8ll/cagr-analyzer.git
   cd cagr-analyzer
   ```

3. Install required R packages:
   ```R
   R -e "install.packages(c('shiny', 'ggplot2', 'lubridate', 'dplyr', 'scales'))"
   ```

4. Run the app:
   ```bash
   Rscript -e "shiny::runApp('.', host = '0.0.0.0', port = 3838)"
   ```

5. Open your browser and navigate to `http://localhost:3838`.

---

### Usage Notes

- Place your CSV files in the working directory or upload them directly through the app.
- CSV files must include `Date` and `Close` columns.
- Use the date range selector to focus your analysis on a specific timeframe.

---

### Contributing

Feel free to open issues or submit pull requests to improve the app!

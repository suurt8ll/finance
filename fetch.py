import pandas as pd
import yfinance as yf
from datetime import datetime, timedelta
import os

# Set default values
default_ticker = 'NVDA'
default_start_date = datetime(datetime.now().year, datetime.now().month, 1)
default_end_date = datetime.now()
default_is_weekend_open = False

# 1. Ask user for Yahoo ticker
ticker = input(f"Enter Yahoo ticker symbol (default: {default_ticker}): ").upper() or default_ticker

# 2. Ask user for start and end date
start_date_input = input(f"Enter start date (YYYY-MM-DD) (default: {default_start_date.strftime('%Y-%m-%d')}): ")
end_date_input = input(f"Enter end date (YYYY-MM-DD) (default: {default_end_date.strftime('%Y-%m-%d')}): ")

# Ask user if the market is open on weekends
is_weekend_open = input("Is the market open on weekends? (yes/no) (default: no): ").strip().lower()

# Convert input strings to datetime objects
start_date = datetime.strptime(start_date_input, '%Y-%m-%d') if start_date_input else default_start_date
end_date = datetime.strptime(end_date_input, '%Y-%m-%d') if end_date_input else default_end_date
is_weekend_open = is_weekend_open == 'yes' if is_weekend_open else default_is_weekend_open

# Define a function to fetch data
def fetch_data(ticker, start, end):
    data = yf.download(ticker, start=start, end=end, group_by='ticker')
    # Ensure that columns are single-level
    if isinstance(data.columns, pd.MultiIndex):
        data.columns = data.columns.get_level_values(1)
    return data

# Fetch the user-requested period
new_data_start = start_date.strftime('%Y-%m-%d')
new_data_end = end_date.strftime('%Y-%m-%d')
print(f"Fetching data from {new_data_start} to {new_data_end}")
all_data = fetch_data(ticker, new_data_start, new_data_end)

# Remove any data from today (since Yahoo updates at the market close)
all_data = all_data[all_data.index < pd.Timestamp(datetime.now().date())]

# 5. Save the data to a file
filename = f'{ticker}.csv'
all_data.to_csv(filename)
print(f"Data saved to {filename}")

from pycoingecko import CoinGeckoAPI
from datetime import datetime, timezone
import pytz


def get_closest_historical_btc_price(date_str, user_timezone):
    cg = CoinGeckoAPI()

    # Get user timezone
    user_tz = pytz.timezone(user_timezone)

    # Check if time is included in date_str, if not then use 00:00
    try:
      target_datetime = datetime.strptime(date_str, "%Y-%m-%d %H:%M")
    except ValueError:
      target_datetime = datetime.strptime(date_str, "%Y-%m-%d")
      target_datetime = target_datetime.replace(hour=0, minute=0)

    # Make datetime timezone aware, then convert it to UTC
    target_datetime_aware = user_tz.localize(target_datetime)
    target_datetime_utc = target_datetime_aware.astimezone(timezone.utc)

    target_timestamp = int(target_datetime_utc.timestamp() * 1000)  # Convert to milliseconds

    # Fetch historical data for the day
    start_of_day_timestamp = int(target_datetime_utc.replace(hour=0, minute=0, second=0, microsecond=0).timestamp())
    historical_data = cg.get_coin_market_chart_range_by_id(
        id='bitcoin',
        vs_currency='eur',
        from_timestamp=start_of_day_timestamp,
        to_timestamp=start_of_day_timestamp + 86400
    )

    prices = historical_data.get('prices', [])
    if not prices:
        raise ValueError("No price data available for the specified date.")

    # Find the closest timestamp
    closest_price = None
    min_diff = float('inf')
    for timestamp_ms, price in prices:
        diff = abs(timestamp_ms - target_timestamp)
        if diff < min_diff:
            min_diff = diff
            closest_price = (price, datetime.fromtimestamp(timestamp_ms / 1000, timezone.utc))
    return closest_price


def get_current_btc_price():
    cg = CoinGeckoAPI()
    current_price = cg.get_price(ids='bitcoin', vs_currencies='eur')['bitcoin']['eur']
    current_timestamp = datetime.now(timezone.utc)
    return current_price, current_timestamp


eur_amount_str = input("Enter the EUR amount: ")
eur_amount = float(eur_amount_str)

target_date_str = input("Enter the target date and time (YYYY-MM-DD or YYYY-MM-DD HH:MM): ")

# Example of use, assuming user is in "Europe/Tallinn"
user_timezone = "Europe/Tallinn"
try:
    closest_price, historical_timestamp = get_closest_historical_btc_price(target_date_str, user_timezone)
    btc_at_date = eur_amount / closest_price
    print(f"At {target_date_str} in {user_timezone}, {eur_amount:.2f} EUR was approximately {btc_at_date:.8f} BTC")
    print(f"Historical BTC price: {closest_price:.2f} EUR at {historical_timestamp}")

except Exception as e:
    print(f"Error fetching historical price: {e}")
    btc_at_date = None

try:
    if btc_at_date is not None:
        current_price, current_timestamp = get_current_btc_price()
        current_value = btc_at_date * current_price
        print(f"Currently, {btc_at_date:.8f} BTC is worth approximately {current_value:.2f} EUR")
        print(f"Current BTC price: {current_price:.2f} EUR at {current_timestamp}")
    else:
        print("Current BTC value not calculated due to historical price error.")

except Exception as e:
    print(f"Error fetching current price: {e}")

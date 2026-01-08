import csv
import random
import time
import os
from pathlib import Path

# Fix paths
base_dir = Path("/home/swimmy/swimmy")
data_dir = base_dir / "data" / "historical"
data_dir.mkdir(parents=True, exist_ok=True)

symbols = ["USDJPY", "EURUSD", "GBPUSD"]
now = int(time.time())

print(f"Generating dummy data in {data_dir} (No Pandas)...")

for sym in symbols:
    file_path = data_dir / f"{sym}_M1.csv"
    print(f"Generating {file_path}...")
    base_price = 150.0 if sym == "USDJPY" else 1.10  # Close enough

    with open(file_path, "w", newline="") as f:
        writer = csv.writer(f)
        # Keeper expects NO header (pandas read_csv with names=...)

        price = base_price
        for i in range(5000):
            ts = now - (5000 - i) * 60
            change = (random.random() - 0.5) * 0.02
            price += change
            open_p = price
            high_p = price + abs(random.random() * 0.05)
            low_p = price - abs(random.random() * 0.05)
            close_p = (high_p + low_p) / 2
            vol = random.randint(10, 100)

            # timestamp,open,high,low,close,tick_volume,spread,real_volume
            writer.writerow([ts, open_p, high_p, low_p, close_p, vol, 0, 0])

print("✅ Dummy data generation complete.")

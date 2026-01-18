import csv
import sys


def check_csv(path):
    print(f"Checking {path}...")
    with open(path, "r") as f:
        reader = csv.reader(f)
        header = next(reader)

        prev_ts = 0
        line_num = 1

        first_bad = None

        for row in reader:
            line_num += 1
            if not row:
                continue
            try:
                ts = int(float(row[0]))  # Handle 1234.0

                if line_num == 2:
                    print(f"Start TS: {ts}")
                    prev_ts = ts
                    continue

                diff = ts - prev_ts

                if diff < 0:
                    print(f"⚠️  Backwards jump at line {line_num}: {prev_ts} -> {ts}")

                if diff > 86400 * 30:  # 30 days gap
                    # Only print first few big gaps
                    if diff > 31536000 * 10:  # 10 years
                        print(
                            f"🚨 HUGE JUMP at line {line_num}: {prev_ts} -> {ts} (+{diff/31536000:.1f} years)"
                        )
                        if not first_bad:
                            first_bad = ts
                        break  # Stop after first huge jump to save time

                prev_ts = ts

            except Exception as e:
                print(f"Error line {line_num}: {e}")

        print(f"Last TS: {prev_ts}")


check_csv("data/historical/USDJPY_M1.csv")

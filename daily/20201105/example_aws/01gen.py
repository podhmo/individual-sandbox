import math
import decimal
from datetime import timedelta, datetime

now = datetime.now()
yesterday = now + timedelta(seconds=-60 * 60 * 5)
time_ranges = []
dt = yesterday
while dt < now:
    dt += timedelta(seconds=900)
    time_ranges.append(dt)
name = "YYY"
for t in time_ranges:
    print(
        f"INSERT {name},type=sin value={math.sin(t.timestamp())} {int(t.timestamp()) * 10 ** 9}"
    )
    print(
        f"INSERT {name},type=cos value={math.cos(t.timestamp())} {int(t.timestamp()) * 10 ** 9}"
    )
    print(
        f"INSERT {name} cos={math.cos(t.timestamp())},sin={math.sin(t.timestamp())} {int(t.timestamp()) * 10 ** 9}"
    )

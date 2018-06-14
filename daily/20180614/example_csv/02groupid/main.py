import time
import sys
from csvresumable import DictReader

with open("input.csv") as rf:
    r = DictReader(rf, key=lambda row: row["userId"])
    for row in r:
        print("start", row["userId"], file=sys.stderr)

        # 重たい処理
        time.sleep(2)
        print(row["name"], int(row["age"]) / int(row["cache"]))
        sys.stdout.flush()

import time
import sys
from csvresumable import DictReader

for filename in ["input.csv", "input2.csv"]:
    with open(filename) as rf:
        r = DictReader(rf, key=lambda row: row["userId"])
        for row in r:
            print("start", row["userId"], file=sys.stderr)

            # 重たい処理
            time.sleep(2)
            if int(row["cache"]) == 0:
                ans = "-"
            else:
                ans = int(row["age"]) / int(row["cache"])
            print(row["name"], ans)
            sys.stdout.flush()

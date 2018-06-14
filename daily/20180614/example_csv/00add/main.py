import json
import time
import sys
from csvresumable import DictReader

with open("input.csv") as rf:
    r = DictReader(rf)
    for row in r:
        print("start", row["id"], file=sys.stderr)

        # 重たい処理
        time.sleep(2)
        print(json.dumps(row))
        sys.stdout.flush()

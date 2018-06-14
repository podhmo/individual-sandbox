import json
import time
import sys
import argparse
from csvresumable import DictReader

parser = argparse.ArgumentParser()
parser.add_argument("--resume", action="store_true")
args = parser.parse_args()

with open("input.csv") as rf:
    r = DictReader(rf, resume=args.resume)
    for row in r:
        print("start", row["id"], file=sys.stderr)

        # 重たい処理
        time.sleep(2)
        print(json.dumps(row))
        sys.stdout.flush()

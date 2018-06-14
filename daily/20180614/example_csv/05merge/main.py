import time
import sys
import csv
import itertools
import csvresumable


def gen(files):
    source = itertools.chain.from_iterable([csv.DictReader(open(f)) for f in files])
    return csvresumable.concat_groupby(source, key=lambda row: row["groupId"])


for group_id, rows in csvresumable.iterate(gen(["input.csv", "input2.csv"])):
    print("start group_id", group_id, file=sys.stderr)
    time.sleep(2)
    print("total", sum(int(row["cache"]) for row in rows))

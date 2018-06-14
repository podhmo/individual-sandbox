import time
import sys
import csv
import itertools
import csvresumable


def gen(files):
    sources = [csv.DictReader(open(f)) for f in files]
    sorted_sources = sorted(itertools.chain.from_iterable(sources), key=lambda row: row["groupId"])
    return itertools.groupby(sorted_sources, key=lambda row: row["groupId"])


for group_id, rows in csvresumable.iterate(gen(["input.csv", "input2.csv"])):
    print("start group_id", group_id, file=sys.stderr)
    time.sleep(2)
    print("total", sum(int(row["cache"]) for row in rows))

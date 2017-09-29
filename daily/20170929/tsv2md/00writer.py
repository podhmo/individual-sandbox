import csv
import sys
from collections import OrderedDict

d = [
    {
        "name": "math",
        "score": 80,
    },
    {
        "name": "english",
        "score": 75,
    },
    {
        "name": "japanese",
        "score": 80,
    },
]

def dump(wf, d, fields):
    print("out", wf.name)
    writer = csv.DictWriter(wf, fields, delimiter="\t", lineterminator="\r\n")
    writer.writeheader()
    for row in d:
        writer.writerow(row)

with open("00score-a.tsv", "w") as wf:
    dump(wf, d, ["name", "score"])

with open("00score-b.tsv", "w") as wf:
    dump(wf, d, ["score", "name"])

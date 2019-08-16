import csv
import sys

fname = sys.argv[1]
with open(fname) as rf:
    for row in csv.DictReader(rf):
        print(row)

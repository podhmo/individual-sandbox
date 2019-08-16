import sys
import csv
import yaml

fname = sys.argv[1]
with open(fname) as rf:
    rows = list(csv.DictReader(rf))
yaml.dump(rows, sys.stdout, sort_keys=False)

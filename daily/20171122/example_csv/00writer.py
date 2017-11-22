import csv
import sys

d = [{"name": "foo", "age": 20}]
w = csv.DictWriter(sys.stdout, list(d[0].keys()), quoting=csv.QUOTE_ALL, delimiter="\t")
w.writeheader()
w.writerows(d)

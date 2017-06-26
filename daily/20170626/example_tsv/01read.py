import sys
import csv

reader = csv.DictReader(sys.stdin, delimiter="\t", doublequote=True, quoting=csv.QUOTE_ALL, strict=True)
for row in reader:
    print(row)

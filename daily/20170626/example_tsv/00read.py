import sys
import csv

reader = csv.reader(sys.stdin, delimiter="\t", doublequote=True, quoting=csv.QUOTE_ALL, strict=True)
for row in reader:
    print(row)

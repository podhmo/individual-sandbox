import csv
import sys


r = csv.DictReader(sys.stdin, delimiter="\t")
print(list(r))

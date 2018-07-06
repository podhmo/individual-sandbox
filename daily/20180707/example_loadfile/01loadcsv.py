import csv
import sys


def loadfile(f=None):
    if f == None:
        return csv.DictReader(sys.stdin)
    with open(f) as rf:
        return csv.DictReader(rf)


# error
xs = loadfile("data.csv")
print(xs)
print(list(xs))  # ValueError: I/O operation on closed file.

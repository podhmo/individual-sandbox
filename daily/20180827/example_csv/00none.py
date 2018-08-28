import sys
import csv

rows = [
    {
        "x": None
    },
    {
        "x": "xxx"
    },
]

w = csv.DictWriter(sys.stdout, fieldnames=["x", "y"], restval="-")
w.writeheader()
w.writerows(rows)

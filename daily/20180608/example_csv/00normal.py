import csv
import sys

w = csv.DictWriter(sys.stdout, ["x", "y", "z"], restval="-")
rows = [
    {
        "x": "10",
        "y": "20",
        "z": "30"
    },
    {
        "x": "100",
        "z": "300"
    },
]
w.writerows(rows)

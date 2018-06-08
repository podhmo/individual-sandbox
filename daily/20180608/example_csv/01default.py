import csv
import sys
from collections import defaultdict

w = csv.DictWriter(sys.stdout, ["x", "y", "z"], restval="-")
rows = [
    defaultdict(str, {"x": "10",
                      "y": "20",
                      "z": "30"}),
    defaultdict(str, {"x": "100",
                      "z": "300"}),
]
w.writerows(rows)

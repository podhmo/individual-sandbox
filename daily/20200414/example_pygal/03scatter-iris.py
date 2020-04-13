import sys
import pygal
import json
from collections import defaultdict


with open("iris.json") as rf:
    data = json.load(rf)

d = defaultdict(list)
for row in data:
    d[row["species"]].append(row)

xy_chart = pygal.XY(stroke=False)
xy_chart.title = "Correlation"
for species, rows in d.items():
    xy_chart.add(species, [(row["sepalWidth"], row["sepalLength"]) for row in rows])

print(xy_chart.render(is_unicode=True))

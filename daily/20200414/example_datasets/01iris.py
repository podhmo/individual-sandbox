import pygal
import pathlib
import json
from collections import defaultdict
import importlib.util

# dataset
spec = importlib.util.find_spec("vega_datasets")
dirpath = spec.submodule_search_locations[0]
with (pathlib.Path(dirpath) / "_data/iris.json").open() as rf:
    data = json.load(rf)

# aggregate
d = defaultdict(list)
for row in data:
    d[row["species"]].append(row)

# render graph
xy_chart = pygal.XY(stroke=False)
xy_chart.title = "Correlation"
for species, rows in d.items():
    xy_chart.add(species, [(row["sepalWidth"], row["sepalLength"]) for row in rows])

print(xy_chart.render(is_unicode=True))

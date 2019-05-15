import matplotlib.pyplot as plt
import math
from dictknife import loading
from collections import defaultdict
import mplcursors

d = defaultdict(dict)
current = loading.loadfile("../ranking.md")
prev = loading.loadfile("../prev-ranking.md")

d = defaultdict(dict)
for row in current:
    d[row["package"]]["last_month"] = math.log(row["last_month"])
for row in prev:
    d[row["package"]]["prev"] = math.log(row["prev"])

rows = sorted(d.items())
fig, ax = plt.subplots()

for k, row in rows:
    y = row["last_month"]
    x = row["prev"]
    ax.scatter(x=x, y=y, alpha=0.5, s=80, label=k)
ax.legend(loc="upper right", bbox_to_anchor=(1.35, 1))

mplcursors.cursor(hover=True)

plt.show()

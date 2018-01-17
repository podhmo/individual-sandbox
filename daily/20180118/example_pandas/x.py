import pandas as pd

ax = df.plot(kind="scatter", x="x", y="y", s=40)
for _, row in df.iterrows():
    print(row.name, row.x, row.y)
    ax.annotate(row.name, row.x, row.y)

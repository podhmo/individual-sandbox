from pandas import DataFrame, Series
from collections import Counter

c = Counter()
c[("a", "x")] = 1
c[("b", "x")] = 2
c[("c", "y")] = 3
print(c)

df = DataFrame(Series(c), columns=["frequency"])
print(df)
df = df.reset_index(level=1)
print(df)
df = df.rename(columns={"level_1": "type"}) # or # df.columns = ["type", "frequency"]
print(df)
print(df.to_csv())

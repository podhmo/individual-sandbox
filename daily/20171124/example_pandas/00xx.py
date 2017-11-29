from pandas import DataFrame, Series
from collections import Counter

c = Counter()
c["a"] = ("x", 1)
c["b"] = ("x", 2)
c["c"] = ("y", 3)
print(c)

df = DataFrame.from_dict(counter, orient="index")
df.columns = ["type", "frequency"]
print(df)
print(df.to_csv())
df = df.reset_index()
print(df)
print(df.to_csv())

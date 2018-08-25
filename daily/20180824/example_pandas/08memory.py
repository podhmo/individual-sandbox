import numpy as np
import pandas as pd

rows = [{"name": "foo", "age": 20}] * 1000
d = {"name": ["foo"] * 1000, "age": [20] * 1000}

df = pd.DataFrame.from_dict(d)

df2 = pd.DataFrame()
df2["name"] = df["name"].astype("category")
df2["age"] = df["age"].astype("uint8")

df3 = pd.DataFrame()
df3["name"] = pd.Series(["foo"] * 1000, dtype="category")
df3["age"] = (np.zeros(1000, dtype="uint8") + 20)

df.info(memory_usage="deep")
print("----------------------------------------")
df2.info(memory_usage="deep")
print("----------------------------------------")
df3.info(memory_usage="deep")

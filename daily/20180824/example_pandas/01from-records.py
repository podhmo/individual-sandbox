import pandas as pd

rows = [
    ("x", "foo", 20),
    ("x", "bar", 21),
    ("y", "boo", 21),
]

df = pd.DataFrame.from_records(rows)
print(df)
print(df.index)
print(df.groupby(0).agg({2: ["min", "max"]}))
print("-")

df = pd.DataFrame.from_records(rows, columns=["group", "name", "age"])
print(df)
print(df.index)
print(df.groupby("group").agg({"age": ["min", "max"]}))

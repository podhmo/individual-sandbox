import pandas as pd

rows = [
    {
        "group": "x",
        "name": "foo",
        "age": 20
    },
    {
        "group": "x",
        "name": "bar",
        "age": 21
    },
    {
        "group": "y",
        "name": "boo",
        "age": 21
    },
]

df = pd.DataFrame.from_records(rows)
print(df)
print(df.index)
print(df.groupby("group").agg({"age": ["min", "max"]}))
print("-")
df = pd.DataFrame.from_records(rows, index=["group", "name"])
print(df)
print(df.index)
print(df.groupby("group").agg({"age": ["min", "max"]}))

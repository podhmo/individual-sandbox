import pandas as pd

d = {
    "group": ["x", "x", "y"],
    "name": ["foo", "bar", "boo"],
    "age": [20, 21, 20],
}

df = pd.DataFrame.from_dict(d)
print(df)
print(df.index)

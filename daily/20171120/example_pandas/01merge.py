import pandas as pd

a = pd.DataFrame({
    "lkey": ["foo", "bar", "baz", "foo"],
    "value": [1, 2, 3, 4],
})
b = pd.DataFrame({
    "rkey": ["foo", "bar", "qux", "bar"],
    "value": [5, 6, 7, 8],
})
df = a.merge(b, left_on="lkey", right_on="rkey", how="outer")
print(df)

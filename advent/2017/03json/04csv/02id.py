import pandas as pd

L = pd.read_csv("with-id.csv", comment="#")
# L = pd.read_csv("with-id.csv", comment="#", dtype={"name": "str", "age": "int64"})
print(L)
print(L["id"])

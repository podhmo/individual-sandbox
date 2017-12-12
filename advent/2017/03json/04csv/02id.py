import pandas as pd

L = pd.read_csv("with-id.csv", comment="#")
L = pd.read_csv("with-id.csv", comment="#", dtype={"id": "object", "name": "object", "age": "int64"})
print(L)
print(L["id"])
print(L["name"])
print(L["age"])

import pandas as pd

L = pd.read_csv("with-comment.csv", comment="#")
print(L["age"])
print(L["age"].mean())

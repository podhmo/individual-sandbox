import pandas
from io import StringIO

csvdata = """\
a,b,c
1,2,3
10,20,30
"""

df = pandas.read_csv(StringIO(csvdata))
print(df)

print("----------------------------------------")
df = df.reindex(copy=False, columns=("c", "b", "a"))
print(df)

print("----------------------------------------")
df.rename(copy=False, inplace=True, columns={"a": "x", "b": "y", "c": "z"})
print(df)

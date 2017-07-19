import pandas as pd
header = ["x", "y"]
rows = [
    [1, 2],
    [3, 4],
]
df = pd.DataFrame(rows, columns=header)
print(df)
rows = [
    [1, 2],
    [3],
]
df = pd.DataFrame(rows, columns=header)
print(df)
rows = [
    [1, 2],
    [3, 4, 5],
]
df = pd.DataFrame(rows, columns=header)
print(df)

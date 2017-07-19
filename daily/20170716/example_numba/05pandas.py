import pandas as pd
header = ["x", "y"]
rows = [
    [1.0, 2],
    [3, 4],
]
df = pd.DataFrame(rows, columns=header)
print(df)
df["x"].astype("float64")
print(df)

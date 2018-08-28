import pandas as pd
import numpy as np

arr = np.arange(6).reshape(2, 3)
columns = ["x", "y", "z"]
df = pd.DataFrame(arr, columns=columns)

print(df)
print(df[["x"]].assign(name="x"))

# col = columns[0]
# print(df[[col]].rename(columns={col: "value"}).assign(name=col))
print(pd.concat([df[[col]].rename(columns={col: "value"}).assign(name=col) for col in columns]))

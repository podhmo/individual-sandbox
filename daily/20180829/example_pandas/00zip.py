import numpy as np
import pandas as pd

mt = np.arange(15).reshape(5, 3)
df = pd.DataFrame(mt, columns=["x", "y", "z"])

print(df)
for _, row in df[["x", "y"]].iterrows():
    print(row["x"])


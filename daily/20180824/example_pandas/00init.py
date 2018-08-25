import numpy as np
import pandas as pd

d = {'col1': [1, 2], 'col2': [3, 4]}
df = pd.DataFrame(data=d)
print(df.dtypes)
print(df)
print(df.describe())
print("----------------------------------------")

df2 = pd.DataFrame(data=d, dtype=np.int8)
print(df2.dtypes)
print(df2)
print("----------------------------------------")

df3 = pd.DataFrame(
    np.random.randint(low=0, high=10, size=(5, 5)),
    columns=['a', 'b', 'c', 'd', 'e'],
)
print(df3.dtypes)
print(df3)

import numpy as np
import pandas as pd


m = np.arange(10).reshape(5, 2)
print(m)

print("()")
print(pd.DataFrame(m))
print(pd.DataFrame(m.transpose()))

print("from_records()")
print(pd.DataFrame.from_records(m))

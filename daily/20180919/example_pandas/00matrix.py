import numpy as np
import pandas as pd

m = np.arange(1, 10).reshape(3, 3)
df = pd.DataFrame(m, columns=["a", "b", "c"])
print(df)

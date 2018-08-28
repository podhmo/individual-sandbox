import scipy.stats as stats
import pandas as pd
import numpy as np
from sklearn.preprocessing import scale

x = np.arange(1, 11, dtype="float64")
x_std = stats.zscore(x)
print(x)
print(x_std)
print(scale(x, with_mean=True, with_std=True))

df = pd.DataFrame(x, columns=["x"])
df["x_std"] = stats.zscore(df["x"])
df["x_std2"] = scale(df["x"])
df["y"] = np.power(2, np.arange(1, 11))
df["y_std"] = stats.zscore(df["y"])
df["y_std2"] = scale(df["y"])
df["z"] = np.power(100, np.arange(1, 11))
df["z_std"] = stats.zscore(df["z"])
df["z_std2"] = scale(df["z"])
df["z_log"] = np.log(df["z"])
df["z_log_std"] = stats.zscore(df["z_log"])
df["z_log_std2"] = scale(df["z_log"])
print(df)

for name in df.columns:
    print(name, "mean", df[name].mean(), "std", df[name].std())

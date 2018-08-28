import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler

x = np.arange(1, 11).reshape(5, 2)
print("0", x)

ss = StandardScaler(copy=True, with_mean=True, with_std=True)
x_std = ss.fit_transform(x)
print("1", x_std)

mms = MinMaxScaler()
x_norm = mms.fit_transform(x)
print("2", x_norm)

from nbreversible import (
    code,
)
import pandas as pd
import numpy as np
import seaborn as sns
# %matplotlib inline


with code():
    xs = np.arange(1,10)
    ys = np.arange(1,10).reshape(9,1)
    m = xs * ys
    df = pd.DataFrame(m)
    df


with code():
    sns.heatmap(df, cmap="Blues")


with code():
    import matplotlib.cm as cm
    cm.cmap_d

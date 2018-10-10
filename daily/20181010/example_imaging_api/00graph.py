import numpy as np
import pandas as pd
import seaborn as sns

N = 50
xs = np.arange(N)
ys = xs + np.random.randn(N) * 3
df = pd.DataFrame({"y": ys, "x": xs})

# sns.set(style="ticks")
# df = sns.load_dataset("anscombe")

ax = sns.lmplot(x="x", y="y", data=df, ci=None, height=4, scatter_kws={"s": 50, "alpha": 1})
from io import BytesIO
o = BytesIO()
ax.savefig(o, format="svg")
print(o.getvalue().decode("utf-8"))

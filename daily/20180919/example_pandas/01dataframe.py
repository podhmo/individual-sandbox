"""# [python][shosai][pandas] dataframeの表示(htmlの表示)を試す"""

import numpy as np
import pandas as pd

# %matplotlib inline
m = np.arange(1, 10).reshape(3, 3)
df = pd.DataFrame(m, columns=["a", "b", "c"])
df
"""
```console
$ nbreversible 01dataframe.py | tee 01dataframe.ipynb
$ jupyter-nbconvert --to markdown --execute 01dataframe.ipynb
$ shosai hatena push 01dataframe.md
```
"""
"""
普通に、raw htmlもhatena blogは対応しているっぽい。なるほど。

この時に利用したファイルの[gist](https://gist.github.com/podhmo/34895004d7bfe8263322bab908629e10)
"""

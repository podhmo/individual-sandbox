    
# [python][shosai][pandas] dataframeの表示(htmlの表示)を試す


```

import numpy as np
import pandas as pd
%matplotlib inline
m = np.arange(1, 10).reshape(3, 3)
df = pd.DataFrame(m, columns=["a", "b", "c"])
df

```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>a</th>
      <th>b</th>
      <th>c</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>1</td>
      <td>2</td>
      <td>3</td>
    </tr>
    <tr>
      <th>1</th>
      <td>4</td>
      <td>5</td>
      <td>6</td>
    </tr>
    <tr>
      <th>2</th>
      <td>7</td>
      <td>8</td>
      <td>9</td>
    </tr>
  </tbody>
</table>
</div>




```console
$ nbreversible 01dataframe.py | tee 01dataframe.ipynb
$ jupyter-nbconvert --to markdown --execute 01dataframe.ipynb
$ shosai hatena push 01dataframe.md
```



普通に、raw htmlもhatena blogは対応しているっぽい。なるほど。

この時に利用したファイルの[gist](https://gist.github.com/podhmo/34895004d7bfe8263322bab908629e10)


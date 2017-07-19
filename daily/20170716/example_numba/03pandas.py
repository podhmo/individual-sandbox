import pandas as pd
from io import StringIO

s = """\
1,2
yunk
1,2,3,4,5,6,7,8,9
1,2,3,4,5,6,7,8,9
"""

xs = pd.read_csv(StringIO(s))
print(xs)

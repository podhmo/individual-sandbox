from io import StringIO
import pandas

tsv = """\
a,b,c,d,e,f,g
1,.1,1.1,5.551115123125783e-17,nan,inf,foo
2,.2,2.2,5.552225223225783e-27,nan,inf,foo
"""

df = pandas.read_csv(StringIO(tsv))
df = df.astype(float, raise_on_error=False)
gdf = df.groupby("g", as_index=False)
print(gdf["a", "b", "c", "d", "e", "f"].sum())
"""
     g    a    b    c             d   e    f
0  foo  3.0  0.3  3.3  5.551115e-17 NaN  inf
"""

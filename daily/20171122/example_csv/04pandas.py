import re
import pandas
import csv
from io import StringIO


def tofloat(d, rx=re.compile("(?:\d+|\d*\.\d+(?:e-\d+)?|nan|inf)")):
    if isinstance(d, (list, tuple)):
        return [tofloat(x, rx=rx) for x in d]
    elif hasattr(d, "keys"):
        r = type(d)()
        for k, v in d.items():
            r[k] = tofloat(v)
        return r
    elif rx.match(d):
        return float(d)
    else:
        return d


def readcsv(rf_or_name, delimiter=","):
    if not hasattr(rf_or_name, "close"):
        with open(rf_or_name) as rf:
            return readcsv(rf_or_name)
    elif hasattr(rf_or_name, "seek"):
        rf = rf_or_name

        r = csv.DictReader(rf, delimiter=delimiter)
        samples = tofloat(next(r))
        del r
        dtype = {k: type(v).__name__ for k, v in samples.items()}

        rf.seek(0)
        return pandas.read_csv(rf, dtype=dtype)
    else:
        raise NotImplementedError("not supported", rf_or_name)


tsv = """\
a,b,c,d,e,f,g
1,.1,1.1,5.551115123125783e-17,nan,inf,foo
2,.2,2.2,5.552225223225783e-27,nan,inf,foo
"""

df = readcsv(StringIO(tsv))
gdf = df.groupby("g", as_index=False)
print(gdf["a", "b", "c", "d", "e", "f"].sum())
"""
     g    a    b    c             d   e    f
0  foo  3.0  0.3  3.3  5.551115e-17 NaN  inf
"""

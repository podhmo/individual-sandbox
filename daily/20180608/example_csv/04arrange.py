from collections import OrderedDict
from io import StringIO
import pandas


def arrange(df, *, mapping):
    used = set()
    candidates = set(df.columns)

    new_columns = []
    for name in mapping.keys():  # mapping must be ordered
        if name in candidates:
            new_columns.append(name)
            used.add(name)

    for name in df.columns:
        if name not in used:
            new_columns.append(name)

    df = df.reindex(copy=False, columns=new_columns)
    df.rename(copy=False, inplace=True, columns=mapping)
    return df


csvdata = """\
a,b,c,d,e
1,2,3,-,?
10,20,30,-,?
"""

df = pandas.read_csv(StringIO(csvdata))
print(df)

mapping = OrderedDict([
    (
        "a",
        "x",
    ),
    (
        "b",
        "y",
    ),
    (
        "c",
        "z",
    ),
    (
        "f",
        "F",
    ),  # missing
])

df = arrange(df, mapping=mapping)
print(df)

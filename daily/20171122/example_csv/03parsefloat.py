import csv
import re
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


tsv = """\
a	b	c	d	e	f	g
1	.1	1.1	5.551115123125783e-17	nan	inf	foo
"""

r = csv.DictReader(StringIO(tsv), delimiter="\t")
print(tofloat(list(r)))
# [{'c': 1.1, 'e': nan, 'd': 5.551115123125783e-17, 'g': 'foo', 'f': inf, 'b': 0.1, 'a': 1.0}]

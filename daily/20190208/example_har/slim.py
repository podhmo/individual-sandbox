import sys
from dictknife import loading
from dictknife.accessing import dictmap

d = loading.loadfile(sys.argv[1], format="json")


def transform(x):
    if isinstance(x, str):
        return x[:140]
    return x


dictmap(transform, d, mutable=True)
loading.dumpfile(d)

from collections import OrderedDict
import yaml
from io import StringIO


def dumps(d):
    io = StringIO()
    yaml.dump(d, io)
    return io.getvalue()

d = OrderedDict()
d["a"] = 1
d["z"] = 2
print(dumps(d))

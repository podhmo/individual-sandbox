import json
from tracemalloc import _format_size
from pympler.asizeof import asizeof

filename = "citylots.json"  # size is 181 Mib
with open(filename) as rf:
    d0 = json.load(rf)

print(type(d0), _format_size(asizeof(d0), False))
# <class 'dict'> 795 MiB

# real    1m31.358s
# user    1m25.421s
# sys     0m4.634s

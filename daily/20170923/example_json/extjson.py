import json
from functools import (
    singledispatch,
    partial,
)


@singledispatch
def encode(o):
    raise TypeError("Object of type '%s' is not JSON serializable" % o.__class__.__name__)


register = encode.register

dump = partial(json.dump, indent=2, ensure_ascii=False, sort_keys=True, default=encode)
dumps = partial(json.dumps, indent=2, ensure_ascii=False, sort_keys=True, default=encode)
load = json.load
loads = json.loads

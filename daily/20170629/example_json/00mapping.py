import json
import sys
from collections import ChainMap
from datetime import datetime

d = ChainMap({
    "name": "foo"
}, ChainMap(
    {
        "age": 20
    },
    ChainMap(
        {
            "now": datetime.now()
        },
    ),
))


def additional_default(d):
    if hasattr(d, "keys"):
        return dict(d)
    elif isinstance(d, datetime):
        return d.isoformat()
    raise TypeError("not supported type: {}".format(type(d)))


json.dump(d, sys.stdout, indent=2, default=additional_default)

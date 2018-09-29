from functools import partial
from dataclasses import is_dataclass, asdict
import json
from json import *  # noqa

# from extjson import *のときに、json moduleと同じsymbolしかexportしない
__all__ = json.__all__


def _custom_default(o):
    if is_dataclass(o):
        return asdict(o)
    raise TypeError(f"{o!r} is not JSON serializable")


dumps = partial(json.dumps, default=_custom_default)
dump = partial(json.dump, default=_custom_default)

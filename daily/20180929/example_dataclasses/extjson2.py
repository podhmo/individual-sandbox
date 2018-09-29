from functools import partial, singledispatch
from typing import Iterable
from dataclasses import is_dataclass, asdict
import json
from json import *  # noqa

# from extjson import *のときに、json moduleと同じsymbolしかexportしない
__all__ = json.__all__


@singledispatch
def default(o):
    for p, fn in default.failbacks:
        if p(o):
            default.register(type(o), fn)
            return fn(o)
    raise TypeError(f"{o!r} is not JSON serializable")


default.failbacks = []  # (predicate, action)
default._seen = set()


def _register_failback(*, predicate, emit, overwrite=False):
    if overwrite or predicate not in default._seen:
        pair = (predicate, emit)
        default.failbacks.append(pair)
        default._seen.add(predicate)
        return pair


default.register_failback = _register_failback


# setup
def _predicate_for_iterable(o):
    return isinstance(o, Iterable)


def _emit_for_iterable(o):
    return list(o)


import datetime
default.register(datetime.datetime, lambda o: o.isoformat())
default.register_failback(predicate=_predicate_for_iterable, emit=_emit_for_iterable)
default.register_failback(predicate=is_dataclass, emit=asdict)
dumps = partial(json.dumps, default=default)
dump = partial(json.dump, default=default)

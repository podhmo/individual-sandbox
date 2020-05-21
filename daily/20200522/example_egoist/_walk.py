from __future__ import annotations
import typing as t
from functools import lru_cache
from collections import deque

if t.TYPE_CHECKING:
    from metashape.analyze.walker import Walker


def get_walker(fns: t.List[t.Callable[..., t.Any]]) -> Walker:
    from metashape.runtime import get_walker as _get_walker
    from metashape.analyze.config import Config
    from egoist.internal._fnspec import fnspec

    dq = deque(fns)

    seen: t.Set[t.Type[t.Any]] = set()
    for fn in fns:
        spec = fnspec(fn)
        for typ in spec.argspec.annotations.values():
            if typ in seen:
                continue
            seen.add(typ)
            dq.append(typ)

    seen = set()  # clear
    classes: t.List[t.Type[t.Any]] = []

    while dq:
        typ = dq.pop()
        if typ in seen:
            continue
        seen.add(typ)

        if isinstance(typ, type):
            classes.append(typ)

        for sub_type in _get_flatten_args(typ):
            dq.append(sub_type)

    return _get_walker(classes, config=Config(option=Config.Option(strict=False)))


@lru_cache(maxsize=256)
def _get_flatten_args(typ: t.Type[t.Any]) -> t.Tuple[t.Type[t.Any]]:
    if not hasattr(typ, "__args__"):
        if typ.__module__ != "builtins":
            return (typ,)
        return ()  # type: ignore

    r: t.Set[t.Type[t.Any]] = set()
    for subtype in typ.__args__:
        r.update(_get_flatten_args(subtype))
    return tuple(sorted(r, key=id))  # type: ignore


def collect_types(fns: t.List[t.Callable[..., t.Any]]) -> t.List[t.Type[t.Any]]:
    return list(get_walker(fns).walk())

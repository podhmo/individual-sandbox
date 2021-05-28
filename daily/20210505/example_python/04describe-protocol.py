import typing as t
from typing_extensions import Protocol


class GetKey(Protocol):
    def get_key(self) -> str:
        """hello"""
        ...


class Materializable(Protocol):
    def materialize(self) -> t.Tuple[GetKey, dict]:
        ...


class Loader(Protocol):
    def load(self) -> t.Any:
        ...

    def dump(self, ob: list) -> None:
        ...


class Loader2(Loader):
    """more strict Loader"""

    def dump(self, ob: t.List[GetKey]) -> None:
        ...


def show(p, *, flatten=True):
    import os
    import inspect

    if bool(os.getenv("DEBUG")):
        import pydoc

        pydoc.help(p)

    candidates = {}
    for current, base in zip(p.mro(), p.mro()[1:]):
        candidates[current] = (base, set(base.__dict__.values()))

    i = 0
    used = set()

    if not flatten:
        raise NotImplementedError("support only flatten=True")

    for current, (base, skipped) in candidates.items():
        if i == 0:
            print(f"protocol {current.__name__}({base.__name__})")
            if current.__doc__ is not None:
                dochead = current.__doc__.split("\n", 1)[0]
                print(f" |  # {dochead}")

        i += 1

        attrs = {}
        for name, attr in current.__dict__.items():
            if name.startswith("_"):
                continue
            if attr in skipped:
                continue
            if name in used:
                continue
            attrs[name] = attr

        for name, attr in attrs.items():
            sig = inspect.signature(attr)
            print(" |")
            print(f" |  {name}{sig}")
            if attr.__doc__ is not None:
                dochead = attr.__doc__.split("\n", 1)[0]
                print(f" |  # {dochead}")
            used.add(name)


# TODO: ModuleScanner
def show_all(*, depth=1):
    import sys

    f = sys._getframe(1)
    modname = f.f_globals["__name__"]
    i = 0

    for name, val in f.f_locals.items():
        if isinstance(val, type) and issubclass(val, Protocol):
            if not hasattr(val, "__module__"):
                continue
            if val.__module__ != modname:
                continue

            if i > 0:
                print()
            show(val)
            i += 1


# show(Loader)
show_all()

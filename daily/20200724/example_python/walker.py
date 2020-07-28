import typing as t
from metashape.analyze.walker import Walker
from metashape.runtime import get_fullname, _guess_kind  # noqa: F401
from metashape.declarative import ignore  # noqa: F401


def get_walker(guess_kind: t.Any = None) -> Walker:
    from metashape.runtime import get_walker

    guess_kind = guess_kind or _guess_kind

    return get_walker(aggressive=True, recursive=True, _depth=2, _guess_kind=guess_kind)

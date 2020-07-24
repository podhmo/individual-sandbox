from metashape.runtime import get_fullname  # noqa: F401
from metashape.declarative import ignore  # noqa: F401


def get_walker():
    from metashape.runtime import get_walker

    return get_walker(aggressive=True, recursive=True, _depth=2)

from __future__ import annotations
import typing


def run(filename: str) -> None:
    pass


typing.get_type_hints(run)  # => {'filename': <class 'str'>, 'return': <class 'NoneType'>}

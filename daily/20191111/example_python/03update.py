from __future__ import annotations
import inspect
import typing


def run(filename: str) -> None:
    pass


spec = inspect.getfullargspec(run)
print(spec.annotations)
spec.annotations.update(typing.get_type_hints(run))
print(spec.annotations)

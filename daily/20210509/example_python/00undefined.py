from __future__ import annotations
import functools
import typing as t
import dataclasses

T = t.TypeVar("T")


@functools.lru_cache(maxsize=None)
def get_undefined(name: str) -> Undefined:
    return Undefined(name)


class Undefined:
    def __init__(self, name: str) -> None:
        print("	hoi", name)
        self.name = name

    def __repr__(self) -> str:
        return f"<undefined of {self.name}>"


print(get_undefined("foo"))
print(get_undefined("foo"))
print(get_undefined("bar"))

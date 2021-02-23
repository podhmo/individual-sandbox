from __future__ import annotations
import typing as t
import logging
from urllib.request import URLError
from metashape.typeinfo import type_string, TypeInfo
from metashape._access import get_fullname


def to_str_fullpath(info: TypeInfo) -> str:
    if info.underlying != info.type_:
        return f"`{info.type_!r}`"
    return get_fullname(info.underlying)


def display(typ: t.Type[t.Any]) -> str:
    print("python:     ", typ)
    print("type_string:", type_string(typ, to_str=to_str_fullpath))


class Person:
    name: str


display(str)
display(int)
print("----------------------------------------")
display(t.List[str])
display(t.Dict[str, t.List[str]])
display(t.Dict[str, t.Dict[str, t.List[str]]])
print("----------------------------------------")
display(Person)
display(t.List[Person])
display(logging.getLogger(__name__).__class__)
display(URLError)

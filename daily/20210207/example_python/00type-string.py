from __future__ import annotations
import typing as t
from metashape.typeinfo import type_string


def display(typ: t.Type[t.Any]) -> str:
    print("python:     ", typ)
    print("type_string:", type_string(typ))


display(str)
display(int)
display(t.List[str])
display(t.Dict[str, t.List[str]])
display(t.Dict[str, t.Dict[str, t.List[str]]])
display(t.Literal["A", "B", "C"])

# python:      <class 'str'>
# type_string: str
# python:      <class 'int'>
# type_string: int
# python:      typing.List[str]
# type_string: list[str]
# python:      typing.Dict[str, typing.List[str]]
# type_string: dict[str, list[str]]
# python:      typing.Dict[str, typing.Dict[str, typing.List[str]]]
# type_string: dict[str, dict[str, list[str]]]
# python:      typing.Literal['A', 'B', 'C']
# type_string: `typing.Literal['A', 'B', 'C']`

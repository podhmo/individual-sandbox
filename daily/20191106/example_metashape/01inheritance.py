from __future__ import annotations
import typing as t
from metashape._access import iterate_props


class BasePerson:
    name: str
    age: int


class _WithMemo:
    memo: t.Optional[str]


class Person(BasePerson, _WithMemo):
    age: t.Optional[int]
    nickname: t.Optional[str]
    _private: int
    parent: t.Optional[Person]


for prop in iterate_props(Person, ignore_private=True):
    print(prop)

# ('memo', typing.Union[str, NoneType], None)
# ('name', <class 'str'>, None)
# ('age', typing.Union[int, NoneType], None)
# ('nickname', typing.Union[str, NoneType], None)
# ('parent', typing.Union[__main__.Person, NoneType], None)

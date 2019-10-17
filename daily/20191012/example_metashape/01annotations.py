from __future__ import annotations
import typing as t
import typing_extensions as tx


class Person:
    name: str
    age: int
    nickname: t.Optional[str] = None
    personality: Personality


class Personality:
    alignment: tx.Literal["chaos", "neutral", "law"]


Person.__annotations__  # => {'name': 'str', 'age': 'int', 'nickname': 't.Optional[str]', 'personality': 'Personality'}
p = Person()
p.name = ""
p.age = 0
p.nickname = None
p.personality = Personality()
p.personality.alignment = "law"

t.get_type_hints(
    Person
)  # => {'name': <class 'str'>, 'age': <class 'int'>, 'nickname': typing.Union[str, NoneType], 'personality': <class '<run_path>.Personality'>}

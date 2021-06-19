from __future__ import annotations
import dataclasses


@dataclasses.dataclass
class Foo:
    name: str
    bar: Bar


@dataclasses.dataclass
class Bar:
    name: str


foo = Foo(name="foo", bar=Bar(name="bar"))
print(foo)
print(dataclasses.asdict(foo))

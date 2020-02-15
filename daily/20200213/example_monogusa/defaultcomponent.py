import dataclasses
from monogusa import default_component


@dataclasses.dataclass
class Foo:
    name: str


@default_component
def foo() -> Foo:
    return Foo(name="foo")

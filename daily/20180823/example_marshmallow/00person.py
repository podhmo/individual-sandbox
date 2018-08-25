import typing as t
import mypy_extensions as mx
from marshmallow import fields
from marshmallow import Schema


class PersonDict(mx.TypedDict):
    name: str
    age: int


class Person(Schema):
    name: t.Any = fields.Str()
    age: t.Any = fields.Int(required=False, missing=0)

    def load(self, data: t.Mapping) -> t.Tuple[PersonDict, t.Mapping]:
        return super().load(data)


class AnotherDict(mx.TypedDict):
    pass


class Another(Schema):
    pass


def use(d: t.Mapping) -> None:
    print(d)


# marshmallow v2
def f() -> None:
    d, err = Person().load({"name": "foo"})
    use(d)


def g() -> None:
    d, err = Another().load({"name": "foo"})
    use(d)

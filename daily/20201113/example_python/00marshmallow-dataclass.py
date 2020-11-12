from __future__ import annotations
import typing as t
import dataclasses
from marshmallow import Schema, fields, post_load, pre_dump


class DataclassSchema(Schema):
    class Meta:
        dataclass = None  # todo: validation

    @post_load
    def to_dataclass(
        self, data: t.Dict[str, t.Any], **ctx: t.Dict[str, t.Any]
    ) -> Person:
        return self.Meta.dataclass(**data)

    @pre_dump
    def omit_none(
        self, d: t.Dict[str, t.Any], **ctx: t.Dict[str, t.Any]
    ) -> t.Dict[str, t.Any]:
        if ctx.get("inplace", False):
            return {
                k: v for k, v in d.items() if v is not None or self.fields[k].allow_none
            }

        for k in list(d):
            if not (d[k] is not None or self.fields[k].allow_none):
                d.pop(k)
        return d

    @pre_dump
    def from_dataclass(
        self, ob: t.Union[object, t.Dict[str, t.Any]], **ctx: t.Dict[str, t.Any]
    ) -> t.Dict[str, t.Any]:
        if hasattr(ob, "keys"):
            return ob
        return dataclasses.asdict(ob)


@dataclasses.dataclass
class Person:
    name: str
    age: t.Optional[int] = None
    father: t.Optional[Person] = None
    mother: t.Optional[Person] = None


class PersonSchema(DataclassSchema):
    class Meta:
        dataclass = Person

    name = fields.String(required=True)
    age = fields.Integer(missing=lambda: 0)
    father = fields.Nested(lambda: PersonSchema())
    mother = fields.Nested(lambda: PersonSchema())

    # TODO: .pyi


def as_dict(schema: DataclassSchema, ob: dataclasses.dataclass) -> t.Dict[str, t.Any]:
    d = schema.omit_none(schema.from_dataclass(ob))
    d = schema.load(d)  # or validate()?
    return schema.dump(d)


data = {"name": "foo"}
print(PersonSchema().load(data))
data = {"name": "foo", "father": {"name": "boo", "age": "20"}}
print(PersonSchema().load(data))
print(dataclasses.asdict(PersonSchema().load(data)))
print("-")
print(PersonSchema().dump(Person(name="foo")))
print(PersonSchema().dump({"name": "foo"}))
print("-")
print(
    (s := PersonSchema()).validate(s.omit_none(dataclasses.asdict(Person(name="foo"))))
)
print(PersonSchema().validate({"name": "foo", "age": "xxx"}))
print("-")
print(as_dict(PersonSchema(), Person(name="foo")))

from marshmallow import Schema, fields


class Person(Schema):
    name = fields.Str()
    age = fields.Int(missing=0)


def use_schema() -> None:
    s = Person()
    d, err = s.load({"name": "foo"})
    print(d)

from marshmallow import Schema, fields


class Person(Schema):
    name = fields.String(required=True)


class People(Person):
    def __init__(self, *args, **kwargs):
        kwargs["many"] = True
        super().__init__(*args, **kwargs)

print(People().load([{"name": "foo"}, {"name": "bar"}]))

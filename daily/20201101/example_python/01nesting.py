from marshmallow import Schema, fields


class Person(Schema):
    name = fields.String(required=True)
    age = fields.Int()
    father = fields.Nested(lambda: Person)
    mother = fields.Nested(lambda: Person)


S = Person
print(
    (s := S()).load(
        {
            "name": "foo",
            "father": {"name": "bar", "age": 20},
            "mother": {"name": "boo", "age": 20},
        }
    )
)

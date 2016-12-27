from marshmallow import(
    Schema,
    fields
)


class Person(Schema):
    name = fields.String(required=True, description='name of something')
    age = fields.Integer(description='age')
    father = fields.Nested('Father')
    mother = fields.Nested('Mother')
    skills = fields.List(fields.Nested('Skill', ), many=True)


class Father(Person):
    pass


class Mother(Person):
    pass


class Skill(Schema):
    name = fields.String(required=True)

data = {"name": "foo", "age": "20", "skills": [{"name": "hai"}]}
print(Person().load(data))

from marshmallow import Schema, fields


class Person(Schema):
    name = fields.String(required=True)
    age = fields.Int(required=True)


class Person2(Person):
    class Meta:
        strict = True


if __name__ == "__main__":
    input_data = {"name": "foo"}
    person = Person().load(input_data)
    print(person)
    # UnmarshalResult(data={'name': 'foo'}, errors={'age': ['Missing data for required field.']})

    input_data = {"name": "foo"}
    person = Person(strict=True).load(input_data)
    # marshmallow.exceptions.ValidationError: {'age': ['Missing data for required field.']}

    input_data = {"name": "foo"}
    person = Person2().load(input_data)
    # marshmallow.exceptions.ValidationError: {'age': ['Missing data for required field.']}

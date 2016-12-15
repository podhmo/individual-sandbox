from marshmallow import Schema


class Person(Schema):
    class Meta:
        fields = ("name", "age")


if __name__ == "__main__":
    input_data = {"name": "foo", "age": 10, "skills": ["a", "b", "c"]}
    person = Person().load(input_data)
    print(person)
    # UnmarshalResult(data={'age': 10, 'name': 'foo'}, errors={})
    print(Person().dump(input_data))
    # MarshalResult(data={'age': 10, 'name': 'foo'}, errors={})

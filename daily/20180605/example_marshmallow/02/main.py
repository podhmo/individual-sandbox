from schema import Person
from collections import namedtuple

PersonModel = namedtuple("PersonModel", "name, age, nick_name")

d = {
    "name": "foo",
    "age": "20",
    "nickName": "F",
}

print(Person().dump(PersonModel(name="foo", age=20, nick_name="F")))
# MarshalResult(data={'name': 'foo', 'nickName': 'F', 'age': 20}, errors={})

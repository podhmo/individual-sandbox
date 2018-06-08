from schema import Person

d = {
    "name": "foo",
    "age": "20",
    "nickName": "F",
}

print(Person().load(d))
# UnmarshalResult(data={'age': 20, 'name': 'foo', 'nick_name': 'F'}, errors={})

from schema import Person

d = {"name": "foo", "age": 20, "father": {"name": "bar", "age": 40}}
s = Person().load(d)
print(s)

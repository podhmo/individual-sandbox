from prestring.python import Module

m = Module()
dataclasses = m.import_("dataclasses")

m.stmt("@{}", dataclasses.dataclass)
with m.class_("Person") as Person:
    m.stmt("name: str")
    m.stmt("age: int")

m.stmt(Person(name="foo", age=20))

print(m)


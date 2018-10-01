import mypy_extensions as mx


class PersonOptional(mx.TypedDict, total=False):
    name: str = "foo"
    age: int = 20


d: PersonOptional = {"name": "foo", "age": 20}  # ok
d["name"]
d.get("name")

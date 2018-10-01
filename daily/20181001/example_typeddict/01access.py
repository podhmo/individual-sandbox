import mypy_extensions as mx


class PersonOptional(mx.TypedDict, total=False):
    name: str
    age: int


d: PersonOptional = {"name": "foo", "age": 20}  # ok
d["name"]
d.get("name")
d["ame"]
d.get("ame")

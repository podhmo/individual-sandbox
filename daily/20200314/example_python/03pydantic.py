import typing as t
from pydantic import BaseModel

if t.TYPE_CHECKING:
    from pydantic import Model


class Person(BaseModel):
    name: str
    age: int
    additionals: t.Dict[str, str] = {}

    @classmethod
    def parse_obj(cls: t.Type["Model"], obj: t.Any) -> "Model":
        parsed = super().parse_obj(obj)
        additionals = parsed.additionals = {}

        fields = set(parsed.fields.keys())
        for k, v in obj.items():
            if k not in fields:
                additionals[k] = v
        return parsed


code = """\
{"name": "foo", "age": 20, "nickname": "F"}
"""

print("input:")
print(code)
data = Person.parse_raw(code)

print("got:")
print(data)

print()
print("output")
print(data.json())

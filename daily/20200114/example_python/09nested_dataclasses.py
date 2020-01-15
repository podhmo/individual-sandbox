import typing as t
import dataclasses


class Nested:
    # wrap original?
    @classmethod
    def from_nested_dict(cls, d, *, _nontype=type(None)):
        fields = dataclasses.fields(cls)  # cache?
        params = {}
        for f in fields:
            if f.name in d:
                val = d[f.name]
                typ = f.type

                # optional:
                if hasattr(typ, "__origin__") and t.get_origin(typ) == t.Union:
                    args = t.get_args(typ)
                    if len(args) == 2 and _nontype in args:
                        typ = args[1] if args[0] == _nontype else args[0]

                if hasattr(typ, "from_nested_dict"):
                    val = typ.from_nested_dict(val)
                params[f.name] = val
        return cls(**params)


@dataclasses.dataclass
class Person(Nested):
    name: str
    age: int


@dataclasses.dataclass
class MorePerson(Person):
    father: t.Optional[Person] = None
    mother: t.Optional[Person] = None


print(Person.from_nested_dict({"name": "foo", "age": 20}))
print(MorePerson.from_nested_dict({"name": "foo", "age": 20}))
print(
    MorePerson.from_nested_dict(
        {"name": "foo", "age": 20, "father": {"name": "foo", "age": 40}}
    )
)

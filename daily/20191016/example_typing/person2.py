import typing as t


class Person:
    age: int
    nickname: t.Optional[str]

    @property
    def name(self) -> str:
        return ""


p = Person()
p.name = "foo"  # AttributeError: can't set attribute
print(p.name)

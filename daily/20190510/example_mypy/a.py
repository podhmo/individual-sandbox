import typing as t


class A:
    name: str
    age: t.Optional[int]

    def __init__(self, name: str, *, age: t.Optional[int] = None) -> None:
        self.name = name
        self.age = age

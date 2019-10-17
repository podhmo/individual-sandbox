import typing as t
import typing_extensions as tx


class HasName(tx.Protocol):
    name: str


class Person:
    name: str

    def __init__(self, name: str) -> None:
        self.name = name


class Display:
    def __init__(self, typ: t.Type[t.Any]) -> None:
        self.typ = typ

    @property
    def name(self) -> str:
        return self.typ.__name__


def get_name(o: HasName) -> str:
    return o.name


def main() -> None:
    get_name(Person())
    get_name(Display(Person))
    # get_name(object())

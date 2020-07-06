from __future__ import annotations
import typing as t
import typing_extensions as tx


class Description:
    def __init__(self, message: str) -> None:
        self.message = message

    def __eq__(self, desc: Description) -> bool:
        return self.__class__ == desc.__class__ and self.message == desc.message


class Person:
    name: tx.Annotated[str, Description("name of person")]
    nickname: str


print(t.get_type_hints(Person))
print(tx.Annotated[int, Description("i")] == tx.Annotated[int, Description("i")])
print(tx.Annotated[int, Description("i")] == int)

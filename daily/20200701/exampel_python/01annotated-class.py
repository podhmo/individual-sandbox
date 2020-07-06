from typing_extensions import Annotated


class Description:
    def __init__(self, message: str) -> None:
        self.message = message


class Person:
    name: Annotated[str, Description("name of person")]


reveal_type(Person.name)

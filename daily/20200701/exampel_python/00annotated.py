from typing_extensions import Annotated


class Description:
    def __init__(self, message: str) -> None:
        self.message = message


def hello(*, name: Annotated[str, Description("name")]) -> None:
    pass


reveal_type(hello)

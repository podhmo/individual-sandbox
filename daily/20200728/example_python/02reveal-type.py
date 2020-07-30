from typing_extensions import Annotated


class Description:
    def __init__(self, description: str) -> None:
        self.description = description


def hello(*, name: Annotated[str, Description("the name of person")]) -> None:
    reveal_type(name)
    print(f"hello {name}")

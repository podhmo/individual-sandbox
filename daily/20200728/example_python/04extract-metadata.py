from typing_extensions import Annotated


class Description:
    def __init__(self, description: str) -> None:
        self.description = description


def hello(*, name: Annotated[str, Description("the name of person")]) -> None:
    print(f"hello {name}")


if __name__ == "__main__":
    from typing_extensions import get_type_hints

    hints = get_type_hints(hello, include_extras=True)

    import typing_inspect

    print(typing_inspect.get_args(hints["name"]))
    # (<class 'str'>,)

    print(hasattr(hints["name"], "__metadata__"))
    # True
    print(hints["name"].__metadata__)
    # (<__main__.Description object at 0x104b9b730>,)

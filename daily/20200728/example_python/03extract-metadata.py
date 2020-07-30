from typing_extensions import Annotated


class Description:
    def __init__(self, description: str) -> None:
        self.description = description


def hello(*, name: Annotated[str, Description("the name of person")]) -> None:
    print(f"hello {name}")


if __name__ == "__main__":
    from typing_extensions import get_type_hints

    print(get_type_hints(hello))
    # {'name': <class 'str'>, 'return': <class 'NoneType'>}

    print(get_type_hints(hello, include_extras=True))
    # {'name': typing_extensions.Annotated[str, <__main__.Description object at 0x10cd1e730>], 'return': <class 'NoneType'>}

    from typing_extensions import get_args, get_origin

    hints = get_type_hints(hello, include_extras=True)
    print(get_args(hints["name"]))
    # (<class 'str'>, <__main__.Description object at 0x106427730>)

    print(get_origin(hints["name"]))
    # <class 'typing_extensions.Annotated'>

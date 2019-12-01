def hello(*, name: str = "world") -> None:
    """hello"""
    print("hello", name)


def bye(*, name: str = "world") -> None:
    """bye"""
    print("bye", name)

def hello(*, name: str) -> None:
    """hello message"""
    print(f"hello {name}")


def bye(*, name: str = "bye") -> None:
    """bye bye"""
    print(f"bye {name}")

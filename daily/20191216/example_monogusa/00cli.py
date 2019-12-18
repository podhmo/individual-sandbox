def hello(*, name: str) -> None:
    print(f"hello {name}")


def bye(*, name: str = "bye") -> None:
    print(f"bye {name}")

def foo(name: str) -> str:
    return name


def main() -> None:
    x = foo("hello")
    print(x.lower())

    y = foo("10")  # as int
    print(y.lower())

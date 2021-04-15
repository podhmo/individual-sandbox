from typing import Any


def greet(prefix: str, *, name: str) -> None:
    print(f"{prefix}, {name}")


def hello(**params: Any) -> None:
    greet("hello", **params)


hello(nam="foo")

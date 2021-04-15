from typing import overload, Any, TYPE_CHECKING


def greet(prefix: str, *, name: str) -> None:
    print(f"{prefix}, {name}")


@overload
def hello(*, name: str) -> None:
    ...


# suppress "Single overload definition, multiple required"
@overload
def hello(*, _: object = ...) -> None:
    ...


def hello(**params: Any) -> None:
    greet("hello", **params)


hello(name="foo")
hello(nam="x")

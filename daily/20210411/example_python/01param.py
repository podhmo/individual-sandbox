from typing import TypedDict
from mypy_extensions import Expand

# https://github.com/python/mypy/issues/4441


class ParamsDict(TypedDict):
    name: str


def greet(prefix: str, *, name: str) -> None:
    print(f"{prefix}, {name}")


def hello(**params: Expand[ParamsDict]) -> None:
    greet("hello", **params)


hello(nam="foo")


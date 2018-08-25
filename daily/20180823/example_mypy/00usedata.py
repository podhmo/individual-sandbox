import typing as t
import mypy_extensions as mx
from data import A

BDict = mx.TypedDict("BDict", {"name": str, "age": int})


class B(A):
    def data(self) -> BDict:
        return t.cast(BDict, {**super().data(), "age": 20})


class C:
    def data(self) -> BDict:
        return {"name": "foo", "age": 20}


def p(x: t.Mapping) -> None:
    print(x)


def main() -> None:
    p(A().data())
    p(B().data())
    p(C().data())

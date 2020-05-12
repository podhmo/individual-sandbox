from __future__ import annotations
from typing import TypeVar, Type, TYPE_CHECKING

TA = TypeVar("TA", bound="A")


class A:
    @classmethod
    def create(cls: Type[TA]) -> TA:
        return cls()


class B(A):
    def method_only_b(self) -> None:
        print("yay")


if TYPE_CHECKING:
    reveal_type(A.create())
    reveal_type(B.create())

B.create().method_only_b()

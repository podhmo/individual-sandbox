from __future__ import annotations

import typing_extensions as tx
import typing as t

T = t.TypeVar("T", bound="A")


class A:
    def new_child(self) -> T:
        return t.cast(T, self.__class__())


class B(A):
    pass


class P(tx.Protocol):
    def new_child(self) -> P:
        ...


a0 = A()
a1: A = a0.new_child()
a2: A = a1.new_child()

b0 = B()
b1: B = b0.new_child()
b2: B = b1.new_child()
# b: B = B().new_child().new_child()

c0 = B()
c1: P = c0.new_child()
c2: P = c1.new_child()

d0 = B()
d1: P = d0.new_child()
d2 = d1.new_child()

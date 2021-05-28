from __future__ import annotations
from gen import Base


class A(Base):
    name: str

    def foo(self, *, nam: str):
        pass


print(A(name="foo"))
print(A(name="foo").name)
print(A(name="foo", age=10))

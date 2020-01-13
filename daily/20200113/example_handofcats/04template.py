from __future__ import annotations
import typing as t
import dataclasses
from _codeobject import Module
from _fake import _FakeModule


@dataclasses.dataclass
class Person:
    name: str
    father: t.Optional[Person] = None
    mother: t.Optional[Person] = None


# transform f -> mf
def f():
    p = Person("foo", father=Person("F"), mother=Person("M"))
    print(p)
    p.name = "FOO"
    p.father.name = "FATHER"
    p.mother.name = "MOTHER"
    print(p)


def mf(m):
    print_ = m.symbol(print)
    Person_ = m.symbol(Person)
    p = m.let("p", Person_("foo", father=Person_("F"), mother=Person_("M")))
    m.stmt(print_(p))
    m.setattr(p, "name", "FOO")
    m.setattr(p.father, "name", "FATHER")
    m.setattr(p.mother, "name", "MOTHER")
    m.stmt(print_(p))


print("original")
f()

print("\n----------------------------------------\n")

print("fake")
m = _FakeModule()
mf(m)

print("\n----------------------------------------\n")


print("codeobject")
m = Module()
mf(m)
print(m)

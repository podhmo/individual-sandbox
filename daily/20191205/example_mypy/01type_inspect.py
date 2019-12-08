from __future__ import annotations
import typing as t


class Person:
    name: "str"


print(t.get_type_hints(Person))
print(t.ForwardRef("str")._evaluate(globals(), locals()))

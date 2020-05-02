from __future__ import annotations
from prestring.text import Module
from prestring.go import goname
from egoist.go.resolver import get_resolver
from walker import walk


class Person:
    name: str
    age: int
    info: Info


class Info:
    memo: str


r = get_resolver()
m = Module(indent="\t")
for item in walk([Person]):
    m.stmt(f"type {goname(item.cls.__name__)} struct {{")
    with m.scope():
        for name, typeinfo, metadata in item.fields:
            gotype = r.resolve_gotype(typeinfo.normalized) # todo: pointer
            m.stmt(f"{goname(name)} {gotype}")
    m.stmt("}")
print(m)

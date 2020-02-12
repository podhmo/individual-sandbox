from metashape.runtime import get_walker
from prestring.python import Module
from prestring.utils import LazyArgumentsAndKeywords, LazyFormat
from typestr import typestr


class Person:
    name: str
    age: int = 0


m = Module()
w = get_walker([Person])
for cls in w.walk():
    name = w.resolver.resolve_typename(cls)

    args = []
    for fieldname, info, metadata in w.for_type(cls).walk():
        # todo: import
        # todo: add function at info
        args.append(LazyFormat("{}: {}", fieldname, typestr(info.raw)))
    with m.def_(name, LazyArgumentsAndKeywords(args)):
        m.stmt("...")
print(m)

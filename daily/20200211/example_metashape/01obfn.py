import typing as t
from metashape.runtime import get_walker
from prestring.python import Module
from prestring.utils import LazyFormat, LazyArgumentsAndKeywords
from typestr import typestr


class Hello:
    name: str
    age: int
    nickname: t.Optional[str] = None


m = Module()
w = get_walker([Hello])
for cls in w.walk():
    name = w.resolver.resolve_typename(cls)
    args = []
    for fieldname, info, metadata in w.for_type(cls).walk():
        # todo: default
        if "default" in metadata:
            args.append(
                LazyFormat(
                    "{}: {} = {}", fieldname, typestr(info.raw), metadata["default"]
                )
            )
        else:
            args.append(LazyFormat("{}: {}", fieldname, typestr(info.raw)))

    with m.def_(name, LazyArgumentsAndKeywords(args)):
        m.stmt("...")
print(m)

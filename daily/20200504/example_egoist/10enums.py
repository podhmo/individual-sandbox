import typing as t
from prestring.go.codeobject import gofile
from egoist.go.resolver import get_resolver
from emit import emit_enums


Op = t.Literal["add", "sub", "mul"]
Op.__name__ = "Op"

m = gofile("main")
resolver = get_resolver(m)
emit_enums(m, Op, resolver=resolver, name="op")

print(m)

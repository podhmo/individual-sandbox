from __future__ import annotations
import typing as t
from prestring.python import Module
from collections import namedtuple

V = namedtuple("V", "name, type_")


def gen_class_code(name: str, fields: t.List[V]) -> str:
    m = Module()
    with m.class_(name):
        _gen_init(m, name=name, fields=fields)
        _gen_repr(m, name=name, fields=fields)
    return str(m)


# TODO: support custom type
def _gen_init(m, *, name: str, fields: t.List[V]) -> None:
    args = [f"{v.name}:{v.type_}" for v in fields]
    with m.def_("__init__", "self", "*", *args):
        for v in fields:
            m.stmt(f"self.{v.name} = {v.name}")


def _gen_repr(m, *, name: str, fields: t.List[V]) -> None:
    with m.def_("__repr__", "self", return_type="str"):
        args = ", ".join(["%s={self.%s!r}" % (v.name, v.name) for v in fields])
        m.stmt('return f"""A({args})"""', args=args)


class Base:
    def __init_subclass__(cls):
        hints = t.get_type_hints(cls)
        m = Module()

        name = cls.__name__
        fields = [
            V(name=k, type_=v.__name__ if hasattr(v, "__name__") else v)
            for k, v in hints.items()
        ]
        _gen_init(m, name=name, fields=fields)
        _gen_repr(m, name=name, fields=fields)
        code = str(m)

        d = {}
        exec(code, d)
        cls.__init__ = d["__init__"]
        cls.__repr__ = d["__repr__"]

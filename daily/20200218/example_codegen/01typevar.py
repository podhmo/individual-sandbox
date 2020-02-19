import typing as t
from prestring.text import Module


class Config:
    name: str
    age: int

    @classmethod
    def emit(cls, m: Module) -> Module:
        m.stmt("// do something")
        return m


def resolve(name: str, typ: t.Type[t.Any]) -> str:
    if issubclass(typ, int):
        return f'flags.Int("{name}", 0, "-")'
    elif issubclass(typ, str):
        return f'flags.String("{name}", "", "-")'
    else:
        raise NotImplementedError(typ)


def gen(typ: t.Type[t.Any], *, m=None, indent="\t"):
    m = m or Module(indent=indent)

    m.stmt("package main")
    m.sep()
    m.stmt("import (")
    with m.scope():
        m.stmt('"flag"')
        m.stmt('"fmt"')
        m.stmt('"log"')
    m.stmt(")")
    m.sep()

    hints = t.get_type_hints(typ)

    m.stmt("var (")
    with m.scope():
        for name, val in hints.items():
            m.stmt("""{} = {},""", name, resolve(name, val))
    m.stmt(")")
    m.sep()

    m.stmt("func main() {")
    with m.scope():
        m.stmt("flag.Parse()")
        m.stmt("if err := run(); err != nil {")
        with m.scope():
            m.stmt('log.Fatalf("+%v", err)')
        m.stmt("}")
    m.stmt("}")
    m.sep()

    m.stmt("func run() error {")
    with m.scope():
        for name in hints.keys():
            m.stmt("{} := *{}", name, name)
        m.sep()
        m = typ.emit(m)
        m.stmt("return nil")
    m.stmt("}")
    return m


m = Module(indent="\t")
print(gen(Config, m=m))

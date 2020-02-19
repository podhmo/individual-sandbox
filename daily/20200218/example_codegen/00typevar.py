import typing as t
import inspect
from prestring.text import Module


def resolve(name: str, typ: t.Type[t.Any]) -> str:
    if issubclass(typ, int):
        return 'flags.Int("{name}", 0, "-")'
    elif issubclass(typ, str):
        return 'flags.String("{name}", "", "-")'
    else:
        raise NotImplementedError(typ)


def gen(fn: t.Callable[..., t.Any], *, m=None, indent="\t"):
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

    spec = inspect.getfullargspec(fn)

    m.stmt("var (")
    with m.scope():
        for name, val in spec.annotations.items():
            if name == "return":
                continue
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
        for name in spec.annotations.keys():
            if name == "return":
                continue
            m.stmt("{} := *{}", name, name)
        m.sep()
        fn(m)
        m.stmt("return nil")
    m.stmt("}")
    return m


def use(*, name: str, age: int) -> None:
    def gen(m):
        pass

    return gen


m = Module(indent="\t")
print(gen(use, m=m))

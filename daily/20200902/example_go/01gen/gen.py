import typing as t
import itertools
from prestring.go.codeobject import Module
from handofcats import as_command


class b:
    pkg = "github.com/pkg/errors"

    @classmethod
    def wrap(cls, m: Module, *, name: str, prev: str) -> str:
        errors = m.toplevel.import_(cls.pkg)
        return f'return {errors.Wrap}({prev}(), "on {name}")'

    @classmethod
    def new(cls, m: Module, *, name: str) -> str:
        fmt = m.toplevel.import_("fmt")
        return f'return {fmt.Errorf}("xxx")'


class a:
    pkg = "errors"

    @classmethod
    def wrap(cls, m: Module, *, name: str, prev: str) -> str:
        fmt = m.toplevel.import_("fmt")
        return f'return {fmt.Errorf}("on {name}: %w", {prev}())'

    @classmethod
    def new(cls, m: Module, *, name: str) -> str:
        errors = m.toplevel.import_(cls.pkg)
        return f'return {errors.New}("xxx")'


def visit(m: Module, *, names: t.List[str]) -> None:
    m.stmt("func run() error {")
    with m.scope():
        m.stmt(f"return {names[0].split('.', 2)[-1]}()")
    m.stmt("}")
    m.sep()

    for name, prev in itertools.zip_longest(names, names[1:]):
        if prev is None:
            visit_core(m, name=name)
        else:
            visit_wrap(m, name=name, prev=prev)
        m.sep()


def visit_core(m: Module, *, name: str) -> None:
    accessor_name, name = name.split(".", 2)
    a = globals()[accessor_name]

    m.stmt(f"func {name}() error {{")
    with m.scope():
        m.stmt(a.new(m, name=name))
    m.stmt("}")


def visit_wrap(m: Module, *, name: str, prev: str) -> None:
    accessor_name, name = name.split(".", 2)
    a = globals()[accessor_name]
    _, prev = prev.split(".", 2)

    m.stmt(f"func {name}() error {{")
    with m.scope():
        m.stmt(a.wrap(m, name=name, prev=prev))
    m.stmt("}")


def gen(*, m: t.Optional[Module] = None, names: t.List[str]) -> Module:
    m = m or Module()

    m.stmt("package main")
    m.sep()
    m.toplevel = m.submodule()
    m.sep()
    m.stmt("func main() {")
    with m.scope():
        m.stmt("if err := run(); err != nil {")
        with m.scope():
            log = m.toplevel.import_("log")
            m.stmt(f'{log.Fatalf}("!!%+v", err)')
        m.stmt("}")
    m.stmt("}")
    m.sep()

    visit(m, names=names)
    return m


@as_command
def run(*, names: t.List[str]):
    m = gen(names=names)
    print(m)

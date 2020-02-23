import typing as t
from handofcats.langhelpers import reify
from _codeobject import Module as _Module
from _codeobject import Symbol


class Module(_Module):
    @reify
    def _import_map(self) -> t.Dict[str, t.Any]:
        return {}

    def import_(self, pkg: str, as_: t.Optional[str] = None) -> "Symbol":
        """like `import <name>`"""
        if as_ is None:
            as_ = pkg.rsplit("/", 1)[-1]

        sym = self._import_map.get(as_)
        if sym is None:
            sym = self._import_map[as_] = Symbol(as_)
            sym.package = pkg
        # todo: for import (
        # "<import path>"
        # )
        return sym


m = Module()
conf = m.import_("github.com/podhmo/apikit/conf")
foo = m.import_("github.com/podhmo/apikit/foo")
with m.func("run", return_="error"):
    filename = Symbol("filename")
    err = Symbol("err")
    c = Symbol("c")
    fooOb = Symbol("fooOb")
    use = Symbol("use")

    m.stmt("filename := *config")
    m.stmt(f"{c}, {err} := {conf.LoadConfig(filename)}")
    # m.stmt("{}, {} := {}", c, err, conf.LoadConfig(filename))
    with m.if_(f"{err} != nil"):
        m.return_(f"{err}")

    m.stmt(f"{fooOb} := {foo.FromConfig(c)}")
    m.return_(f"{use}({fooOb})")
print(m)

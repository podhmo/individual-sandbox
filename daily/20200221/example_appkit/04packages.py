import typing as t
from handofcats.langhelpers import reify
from prestring.go import Module as _Module
from _codeobject import CodeobjectModule
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
co = CodeobjectModule(m)

conf = co.import_("github.com/podhmo/apikit/conf")
foo = co.import_("github.com/podhmo/apikit/foo")

with m.func("run", return_="error"):
    filename = co.let("filename", "*config")

    fooOb = co.symbol("fooOb")
    use = co.symbol("use")

    c, err = co.letN(("c", "err"), conf.LoadConfig(filename))
    with m.if_(f"{err} != nil"):
        m.return_(f"{err}")

    fooOb = co.let("fooOb", foo.FromConfig(c))
    m.return_(f"{use}({fooOb})")
print(m)

import typing as t
from prestring.utils import reify
from prestring.go import Module as _Module
from prestring.codeobject import Symbol


class Module(_Module):
    @reify
    def _import_map(self) -> t.Dict[str, t.Any]:
        return {}

    def import_(self, pkg: str, as_: t.Optional[str] = None) -> Symbol:
        """like `import <name>`"""
        if as_ is None:
            as_ = pkg.rsplit("/", 1)[-1]

        sym = self._import_map.get(as_)
        if sym is None:
            sym = self._import_map[as_] = Symbol(as_)
            # sym.package = pkg
        # todo: for import (
        # "<import path>"
        # )
        return sym

import typing as t
from egoist.internal._fnspec import fnspec
from prestring.go.codeobject import Module, Symbol, goname

m = None


def _literal(s: str) -> t.Any:
    import json
    from prestring.utils import UnRepr

    return UnRepr(json.dumps(s))


def codeobject(emit_fn: t.Callable[..., t.Any]) -> t.Callable[..., Symbol]:
    def _emit(receiver: str) -> Symbol:
        global m

        spec = fnspec(emit_fn)
        methodname = goname(spec.name)
        self = m.symbol(receiver.lstrip("* ")[0].lower())

        # todo: type -> gotype
        args = [m.symbol(name) for name, _, _, in spec.arguments]
        return_type = "string"

        with m.method(
            f"{self} *{receiver}",
            methodname,
            *[f"{x} string" for x in args],
            returns=return_type,
        ):
            emit_fn(self, *args)
        return m.symbol(methodname)

    return _emit


@codeobject
def emitHello(self, name: str) -> str:
    global m
    fmt = m.import_("fmt")
    m.return_(fmt.Printf(_literal("Hello %s\n"), name))


m = Module()
m.import_("")
emitHello(receiver="Greeter")
print(m)

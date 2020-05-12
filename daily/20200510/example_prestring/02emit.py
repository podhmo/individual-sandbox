import typing as t
from egoist.internal._fnspec import fnspec
from prestring.go.codeobject import Module, Symbol, goname

m = None


class MethodEmitter:
    def __init__(self, typ: t.Type[t.Any]) -> None:
        typename = typ.__name__
        self.typename = typename
        self.receiver = typename.lstrip(" *").lower()[0]

    def emit(self, emit_fn: t.Callable[..., t.Any]) -> Symbol:
        global m
        spec = fnspec(emit_fn)
        methodname = goname(spec.name)

        # todo: type -> gotype
        args = [m.symbol(name) for name, _, _, in spec.arguments]
        return_type = "string"

        with m.method(
            f"{self.typename} *{self.receiver}",
            methodname,
            *[f"{x} string" for x in args],
            returns=return_type,
        ):
            emit_fn(m.symbol(self.receiver), *args)
        return m.symbol(methodname)


def _literal(s: str) -> t.Any:
    import json
    from prestring.utils import UnRepr

    return UnRepr(json.dumps(s))


gomethod = staticmethod


class Greeter:
    @gomethod
    def emitHello(self, message: str) -> str:
        global m
        fmt = m.import_("fmt")
        m.return_(fmt.Printf(_literal("Hello %s\n"), message))


m = Module()
m.package("greeter")
m.import_("")
emitter = MethodEmitter(Greeter)
emitter.emit(Greeter.emitHello)
print(m)

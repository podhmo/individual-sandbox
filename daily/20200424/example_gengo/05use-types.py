import typing as t
import typing_extensions as tx
from graph import Builder, Graph, primitive
from graph import topological_sorted

from gogen._fnspec import fnspec
from handofcats import as_command
from prestring.go import Module as _Module
from prestring.codeobject import CodeObjectModuleMixin, Symbol


class Module(CodeObjectModuleMixin, _Module):
    assign_op = ":="


class Config:
    pass


class X:
    pass


class Y:
    pass


class Z:
    pass


class GoError:
    name = "err"

    @classmethod
    def emit(self, m: Module, err: Symbol) -> None:
        with m.if_(f"{err} != nil"):
            m.return_(err)


class GoTeardown:
    name = "teardown"

    @classmethod
    def emit(self, m: Module, teardown: Symbol) -> None:
        m.stmt(f"defer {teardown}()")


def NewConfig(filename: str) -> t.Tuple[Config, GoError]:
    pass


def NewX(config: Config) -> X:
    pass


def NewY(config: Config) -> Y:
    pass


def NewZ(x: X, y: Y) -> t.Tuple[Z, GoTeardown]:
    pass


class Metadata(tx.TypedDict, total=False):
    provider: str
    return_type: t.Type[t.Any]
    component_type: t.Type[t.Any]


def parse(fn: t.Callable[..., t.Any]) -> t.Tuple[str, t.List[str], Metadata]:
    spec = fnspec(fn)

    depends = [
        primitive(name) if typ.__module__ == "builtins" else typ.__name__
        for name, typ, _ in spec.arguments
    ]

    return_type = spec.return_type
    if not hasattr(return_type, "__origin__"):
        component_type = return_type
    else:
        assert return_type.__origin__ == tuple, return_type.__origin__
        component_type, _ = t.get_args(spec.return_type)

    metadata: Metadata = {
        "provider": fn.__name__,
        "return_type": return_type,
        "component_type": component_type,
    }
    return {"name": component_type.__name__, "depends": depends, "metadata": metadata}


def emit(g: Graph) -> Module:
    i = 0
    m = Module()
    variables: t.Dict[int, Symbol] = {}

    # TODO: with type
    # TODO: name
    root_args = [f"{node.name} string" for node in g.nodes if node.is_primitive]
    with m.func("run", *root_args, return_="error"):
        for node in topological_sorted(g):
            if node.is_primitive:
                variables[node.uid] = m.symbol(node.name)
                continue

            metadata = t.cast(Metadata, node.metadata)
            return_type = metadata.get("return_type", "")

            return_types = list(t.get_args(return_type) or [return_type])
            var_names = [
                f"v{i}",
                *[getattr(typ, "name", typ.__name__) for typ in return_types[1:]],
            ]
            provider_callable = m.symbol(metadata.get("provider") or node.name)

            args = [variables[dep.uid] for dep in node.depends]
            variables[node.uid], *extra_vars = m.letN(
                var_names, provider_callable(*args)
            )
            if extra_vars:
                for sym, typ in zip(extra_vars, return_types[1:]):
                    if hasattr(typ, "emit"):
                        typ.emit(m, sym)

            i += 1
        m.return_("nil")
    return m


@as_command  # type: ignore
def run() -> None:
    b = Builder()

    b.add_node(**parse(NewConfig))
    b.add_node(**parse(NewX))
    b.add_node(**parse(NewY))
    b.add_node(**parse(NewZ))
    g = b.build()
    print(emit(g))

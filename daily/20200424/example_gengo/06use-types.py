import typing as t
import typing_extensions as tx
from graph import Builder, Graph, primitive
from graph import topological_sorted

from gogen._fnspec import fnspec
from handofcats import as_command
from prestring.go.codeobject import Module, Symbol


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
    priority = 10

    @classmethod
    def emit(self, m: Module, err: Symbol) -> None:
        with m.if_(f"{err} != nil"):
            m.return_(err)


class GoTeardown:
    name = "teardown"
    priority = 1

    @classmethod
    def emit(self, m: Module, teardown: Symbol) -> None:
        m.stmt(f"defer {teardown}()")


def NewConfig(filename: str) -> t.Tuple[Config, GoError]:
    pass


def NewX(config: Config, version: int) -> X:
    pass


def NewY(config: Config) -> t.Tuple[Y, GoTeardown, GoError]:
    pass


def NewZ(x: X, y: Y) -> t.Tuple[Z, GoTeardown]:
    pass


class Metadata(tx.TypedDict, total=False):
    provider: str
    return_type: t.Type[t.Any]
    type_: t.Type[t.Any]


def parse(fn: t.Callable[..., t.Any]) -> t.Tuple[str, t.List[str], Metadata]:
    spec = fnspec(fn)

    depends = [
        primitive(name, metadata={"type_": typ})
        if typ.__module__ == "builtins"
        else typ.__name__
        for name, typ, _ in spec.arguments
    ]

    return_type = spec.return_type
    if not hasattr(return_type, "__origin__"):
        component_type = return_type
    else:
        assert return_type.__origin__ == tuple, return_type.__origin__
        component_type, *_ = t.get_args(spec.return_type)

    metadata: Metadata = {
        "provider": fn.__name__,
        "return_type": return_type,
        "type_": component_type,
    }
    return {
        "name": component_type.__name__,
        "depends": depends,
        "metadata": metadata,
    }


def _get_args(g: Graph) -> t.List[str]:
    root_args = []
    for node in g.nodes:
        if not node.is_primitive:
            continue
        metadata = t.cast(Metadata, node.metadata)
        if issubclass(metadata["type_"], int):
            gotype = "int"
        else:
            gotype = "string"
        root_args.append(f"{node.name} {gotype}")
    return root_args


def emit(g: Graph) -> Module:
    # TODO: name
    # TODO: import_
    i = 0
    m = Module()
    variables: t.Dict[int, Symbol] = {}

    with m.func("run", *_get_args(g), return_="error"):
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
                for sym, typ in sorted(
                    zip(extra_vars, return_types[1:]),
                    key=lambda pair: getattr(pair[1], "priority", 5),
                    reverse=True,
                ):
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

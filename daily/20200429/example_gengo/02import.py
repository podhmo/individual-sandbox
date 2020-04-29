from __future__ import annotations
import typing as t
import typing_extensions as tx
from graph import Builder, Graph, primitive
from graph import topological_sorted

from handofcats import as_command
from prestring.go.codeobject import Module, Symbol
from egoist.internal._fnspec import fnspec, Fnspec


class Message:
    gopackage = "m/internal"


class Greeter:
    gopackage = "m/internal"


class Event:
    gopackage = "m/internal"


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


def NewMessage() -> Message:
    pass


def NewGreeter(message: Message) -> Greeter:
    pass


def NewEvent(g: Greeter) -> t.Tuple[Event, GoError]:
    pass


class Metadata(tx.TypedDict, total=False):
    return_type: t.Type[t.Any]
    component_type: t.Type[t.Any]
    fnspec: Fnspec


def parse(fn: t.Callable[..., t.Any]) -> t.Tuple[str, t.List[str], Metadata]:
    spec = fnspec(fn)

    depends = [
        primitive(name, metadata={"component_type": typ})
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
        "return_type": return_type,
        "component_type": component_type,
        "fnspec": spec,
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
        if issubclass(metadata["component_type"], int):
            gotype = "int"
        else:
            gotype = "string"
        root_args.append(f"{node.name} {gotype}")
    return root_args


def get_module(name: str) -> Module:
    from prestring.go.codeobject import go_file

    return go_file(name)


def emit(m: Module, g: Graph) -> Symbol:
    # TODO: name
    # TODO: import_
    i = 0
    variables: t.Dict[int, Symbol] = {}

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

        spec: Fnspec = metadata.get("fnspec")
        provider_callable: t.Optional[Symbol] = None
        if spec is not None:
            pkg_prefix = m.import_(metadata["component_type"].gopackage)
            provider_callable = m.symbol(f"{pkg_prefix}.{spec.name}")
        if provider_callable is None:
            provider_callable = m.symbol(spec.name if spec else node.name)

        args = [variables[dep.uid] for dep in node.depends]
        variables[node.uid], *extra_vars = m.letN(var_names, provider_callable(*args))

        if extra_vars:
            for sym, typ in sorted(
                zip(extra_vars, return_types[1:]),
                key=lambda pair: getattr(pair[1], "priority", 5),
                reverse=True,
            ):
                if hasattr(typ, "emit"):
                    typ.emit(m, sym)

        i += 1
    return variables[node.uid]


@as_command  # type: ignore
def run() -> None:
    b = Builder()

    b.add_node(**parse(NewMessage))
    b.add_node(**parse(NewGreeter))
    b.add_node(**parse(NewEvent))

    g = b.build()
    m = get_module("main")

    with m.func("main"):
        m.stmt("run()")

    with m.func("run", *_get_args(g), return_="error"):
        component = emit(m, g)
        m.stmt(f"{component}.Start()")
        m.return_("nil")
    print(m)

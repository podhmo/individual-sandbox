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


class err:
    pass


class cleanup:
    pass


def NewConfig(filename: str) -> t.Tuple[Config, err]:
    pass


def NewX(config: Config) -> X:
    pass


def NewY(config: Config) -> Y:
    pass


def NewZ(x: X, y: Y) -> t.Tuple[Z, cleanup]:
    pass


ProviderType = tx.Literal["", "with-err", "with-cleanup", "with-cleanup-err"]


class Metadata(tx.TypedDict, total=False):
    provider_type: ProviderType
    provider: str


def parse(fn) -> t.Tuple[str, t.List[str], Metadata]:
    spec = fnspec(fn)

    depends = [
        primitive(name) if typ.__module__ == "builtins" else typ.__name__
        for name, typ, _ in spec.arguments
    ]

    metadata: Metadata = {}
    return_type = spec.return_type
    if not hasattr(return_type, "__origin__"):
        component_type = return_type
    else:
        assert return_type.__origin__ == tuple, return_type.__origin__
        component_type, *extra_types = t.get_args(spec.return_type)
        if extra_types:
            provider_type = "with-{}".format("-".join(x.__name__ for x in extra_types))
            metadata["provider_type"] = provider_type

    return {"name": component_type.__name__, "depends": depends, "metadata": metadata}


def resolve(g: Graph) -> Module:
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

            args = [variables[dep.uid] for dep in node.depends]

            metadata = t.cast(Metadata, node.metadata)
            return_type = metadata.get("return_type", "")
            provider = m.symbol(metadata.get("provider") or node.name)

            if return_type == "":
                (variables[node.uid],) = m.letN([f"v{i}"], provider(*args))
            elif return_type == "with-err":
                variables[node.uid], err = m.letN([f"v{i}", "err"], provider(*args))
                with m.if_("err != nil"):
                    m.return_("err")
            elif return_type == "with-cleanup":
                variables[node.uid], cleanup = m.letN(
                    [f"v{i}", "cleanup"], provider(*args)
                )
                m.stmt("defer cleanup()")
            elif return_type == "with-cleanup-err":
                variables[node.uid], cleanup, err = m.letN(
                    [f"v{i}", "cleanup", "err"], provider(*args)
                )
                with m.if_("err != nil"):
                    m.return_("err")
                m.stmt("defer cleanup()")
            else:
                raise ValueError(f"unexpected return_type {return_type}")
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
    print(resolve(g))

import typing as t
import typing_extensions as tx
from graph import Builder, Graph
from graph import topological_sorted
from handofcats import as_command
from prestring.go import Module as _Module
from prestring.codeobject import CodeObjectModuleMixin, Symbol


class Module(CodeObjectModuleMixin, _Module):
    assign_op = ":="


ReturnType = tx.Literal["", "with-err", "with-cleanup", "with-cleanup-err"]


class Metadata(tx.TypedDict, total=False):
    return_type: ReturnType
    provider: str


# metadata :: return_type, provider


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

            args = [variables[dep.uid] for dep in node.deps]

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

    b.add_node("Message", metadata={"provider": "NewMessage"})
    b.add_node("Greeter", deps=["Message"], metadata={"provider": "NewGreeter"})
    b.add_node("Event", deps=["Greeter"], metadata={"provider": "NewEvent"})

    g = b.build()
    print(resolve(g))

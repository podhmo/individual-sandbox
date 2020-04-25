import typing as t
import typing_extensions as tx
from graph import Builder, Graph, primitive
from graph import topological_sorted
from handofcats import as_command
from prestring.go import Module as _Module
from prestring.codeobject import CodeObjectModuleMixin, Symbol


class Module(CodeObjectModuleMixin, _Module):
    assign_op = ":="


ReturnType = tx.Literal["", "with-err", "with-cleanup", "with-cleanup-err"]


def resolve(g: Graph) -> Module:
    i = 0
    m = Module()
    variables: t.Dict[int, Symbol] = {}

    # TODO: with type
    # TODO: name
    original_args = [f"{node.name} string" for node in g.nodes if node.is_primitive]
    with m.func("run", *original_args, return_="error"):
        for node in topological_sorted(g):
            if node.is_primitive:
                variables[node.uid] = m.symbol(node.name)
                continue

            args = []
            for dep in node.depends:
                args.append(variables[dep.uid])
            m.letN
            return_type = node.metadata.get("return_type", "")
            if return_type == "":
                [variables[node.uid]] = m.letN([f"v{i}"], m.symbol(node.name)(*args))
            elif return_type == "with-err":
                variables[node.uid], err = m.letN(
                    (f"v{i}", "err"), m.symbol(node.name)(*args)
                )
                with m.if_("err != nil"):
                    m.return_("err")
            elif return_type == "with-cleanup":
                variables[node.uid], cleanup = m.letN(
                    (f"v{i}", "cleanup"), m.symbol(node.name)(*args)
                )
                m.stmt("defer cleanup()")
            elif return_type == "with-cleanup-err":
                variables[node.uid], cleanup, err = m.letN(
                    (f"v{i}", "cleanup", "err"), m.symbol(node.name)(*args)
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

    b.add_node(
        "Config", depends=[primitive("filename")], metadata={"return_type": "with-err"},
    )
    b.add_node("X", depends=["Config"])
    b.add_node("Y", depends=["Config"])
    b.add_node("Z", depends=["X", "Y"], metadata={"return_type": "with-cleanup"})

    g = b.build()
    print(resolve(g))

import typing as t
import typing_extensions as tx
from graph import Builder, Graph, primitive
from graph import topological_sorted
from handofcats import as_command

ReturnType = tx.Literal["", "with-err", "with-teardown", "with-teardown-err"]


def resolve(g: Graph):
    from prestring.python import Module as _Module
    from prestring.codeobject import CodeObjectModuleMixin, Symbol

    class Module(_Module, CodeObjectModuleMixin):
        pass

    i = 0
    m = Module()
    variables: t.Dict[int, Symbol] = {}
    for node in topological_sorted(g):
        if node.is_primitive:
            variables[node.uid] = m.symbol(node.name)
            continue

        args = []
        for dep in node.deps:
            args.append(variables[dep.uid])

        return_type = node.metadata.get("return_type", "")
        variables[node.uid] = m.let(f"v{i}", m.symbol(node.name)(*args))
        i += 1
    print(m)


@as_command  # type: ignore
def run() -> None:
    b = Builder()

    b.add_node("Config", deps=[primitive("filename")], metadata={"return_type": "with-err"})
    b.add_node("X", deps=["Config"])
    b.add_node("Y", deps=["Config"])
    b.add_node("Z", deps=["X", "Y"], metadata={"return_type": "with-teradown"})

    g = b.build()
    print(resolve(g))

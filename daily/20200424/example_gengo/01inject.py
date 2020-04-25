import typing as t
from graph import Builder, Graph, primitive
from graph import topological_sorted
from handofcats import as_command
from prestring.python import Module as _Module
from prestring.codeobject import CodeObjectModuleMixin, Symbol


class Module(_Module, CodeObjectModuleMixin):
    pass


def emit(g: Graph) -> Module:
    i = 0
    m = Module()
    variables: t.Dict[int, Symbol] = {}
    for node in topological_sorted(g):
        if node.is_primitive:
            variables[node.uid] = m.symbol(node.name)
            continue

        args = []
        for dep in node.depends:
            args.append(variables[dep.uid])
        variables[node.uid] = m.let(f"v{i}", m.symbol(node.name)(*args))
        i += 1
    return m


@as_command  # type: ignore
def run() -> None:
    b = Builder()

    b.add_node("Config", depends=[primitive("filename")])
    b.add_node("X", depends=["Config"])
    b.add_node("Y", depends=["Config"])
    b.add_node("Z", depends=["X", "Y"])

    g = b.build()
    print(emit(g))

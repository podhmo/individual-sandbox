import typing as t
import contextlib
from prestring.text import Module as _Module

from graph import Graph


class Module(_Module):
    @contextlib.contextmanager
    def block(self, expr: str = "") -> t.Iterator[None]:
        line = []
        if expr:
            line.append(expr)
        line.append("{")
        self.stmt(" ".join(line))
        with self.scope():
            yield
        self.stmt("}")


def visualize(g: Graph) -> Module:
    """generate dot file (graphviz)"""
    m = Module()
    with m.block(f"digraph {g.name}"):
        # nodes
        m.stmt("// nodes")
        for node in g.nodes:
            # TODO: change shape by kind
            # https://www.graphviz.org/doc/info/shapes.html
            shape = "plain" if node.kind == "primitive" else "oval"
            m.stmt(f"""g{node.uid} [label="{node.name}", shape={shape}];""")

        m.stmt("")

        # edges
        m.stmt("// edges")
        for node in g.nodes:
            for dep in node.depends:
                m.stmt(f"g{dep.uid} -> g{node.uid};")
    return m

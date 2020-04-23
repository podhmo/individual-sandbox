from __future__ import annotations
import typing as t
import dataclasses
import contextlib
from collections import defaultdict
from prestring.text import Module as _Module

NodeKind = t.Literal["primitive", "component"]


@dataclasses.dataclass(frozen=True, eq=True)
class Seed:
    name: str
    kind: NodeKind


@dataclasses.dataclass(frozen=True, repr=False)
class Node:
    uid: int
    name: str
    kind: NodeKind
    deps: t.List[Node]

    def __repr__(self) -> str:
        return f"<Node uid={self.uid} deps={len(self.deps)} name={self.name!r}>"


def primitive(name: str) -> Seed:
    return Seed(name=name, kind="primitive")


def component(name: str) -> Seed:
    return Seed(name=name, kind="component")


class Builder:
    def __init__(self) -> None:
        self.deps_map: t.Dict[str, t.Set[str]] = defaultdict(set)
        self.node_map: t.Dict[str, Seed] = {}
        self.uid_map: t.Dict[str, int] = defaultdict(lambda: len(self.uid_map))

    def add_node(self, name: str, *, deps: t.List[t.Union[Seed, str]]) -> None:
        dep_node_map: t.List[Seed] = []
        for name_or_node in deps:
            if isinstance(name_or_node, Seed):
                dep_node_map.append(name_or_node)
            else:
                dep_node_map.append(component(name_or_node))

        self.deps_map[name].update([dep.name for dep in dep_node_map])
        self.uid_map[name]
        for dep in dep_node_map:
            if dep.name in self.node_map:
                assert self.node_map[dep.name] == dep
                continue

            self.node_map[dep.name] = dep
        self.node_map[name] = component(name)

    def build(self, *, name: str = "G") -> Graph:
        node_map: t.Dict[int, Node] = {}

        def create(seed: Seed) -> Node:
            uid = self.uid_map[seed.name]

            if uid in node_map:
                return node_map[uid]

            deps: t.List[Node] = []
            node = node_map[uid] = Node(
                name=seed.name, kind=seed.kind, uid=uid, deps=deps,
            )
            deps.extend(
                [create(self.node_map[name]) for name in self.deps_map[seed.name]]
            )
            return node

        for seed in self.node_map.values():
            create(seed)
        return Graph(node_map, name=name)


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


class Graph:
    def __init__(self, node_map: t.Dict[int, Node], *, name: str) -> None:
        self.name = name
        self._node_map: t.Dict[int, Node] = node_map
        self.nodes = sorted(self._node_map.values(), key=lambda node: node.uid)

    def __repr__(self) -> str:
        return f"<Graph len={len(self.nodes)}>"


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
            for dep in node.deps:
                m.stmt(f"g{dep.uid} -> g{node.uid};")
    return m

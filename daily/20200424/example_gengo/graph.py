from __future__ import annotations
import typing as t
import typing_extensions as tx
import dataclasses
import contextlib
from collections import defaultdict
from prestring.text import Module as _Module

NodeKind = tx.Literal["primitive", "component"]


@dataclasses.dataclass(frozen=True, eq=True)
class Seed:
    name: str
    kind: NodeKind


@dataclasses.dataclass(frozen=True, repr=False)
class Node:
    uid: int
    name: str
    kind: NodeKind
    depends: t.List[Node]  # todo: immutable
    metadata: t.Dict[str, t.Any]  # todo: immutable

    def __repr__(self) -> str:
        return f"<Node uid={self.uid} depends={len(self.depends)} name={self.name!r}>"

    @property
    def is_primitive(self) -> bool:
        return self.kind == "primitive"

    @property
    def is_component(self) -> bool:
        return self.kind == "component"


def primitive(name: str) -> Seed:
    return Seed(name=name, kind="primitive")


def component(name: str) -> Seed:
    return Seed(name=name, kind="component")


class Builder:
    def __init__(self) -> None:
        # todo: str -> int
        self.depends_map: t.Dict[str, t.Set[str]] = defaultdict(set)
        self.node_map: t.Dict[str, Seed] = {}
        self.uid_map: t.Dict[str, int] = defaultdict(lambda: len(self.uid_map))
        self.metadata_map: t.Dict[str, t.Dict[str, t.Any]] = defaultdict(dict)

    def add_node(
        self,
        name: str,
        *,
        depends: t.Optional[t.List[t.Union[Seed, str]]] = None,
        metadata: t.Optional[t.Dict[str, t.Any]] = None,
    ) -> None:
        depends = depends or []
        dep_node_map: t.List[Seed] = []

        for name_or_node in depends:
            if isinstance(name_or_node, Seed):
                dep_node_map.append(name_or_node)
            else:
                dep_node_map.append(component(name_or_node))

        self.depends_map[name].update([dep.name for dep in dep_node_map])
        self.uid_map[name]
        if metadata is not None:
            self.metadata_map[name] = metadata

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

            depends: t.List[Node] = []
            node = node_map[uid] = Node(
                name=seed.name,
                kind=seed.kind,
                uid=uid,
                depends=depends,
                metadata=self.metadata_map[seed.name],
            )
            depends.extend(
                [create(self.node_map[name]) for name in self.depends_map[seed.name]]
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


def topological_sorted(g: Graph) -> t.List[Node]:
    seen: t.Set[int] = set()
    r: t.List[Node] = []

    def visit(node: Node) -> None:
        if node.uid in seen:
            return
        seen.add(node.uid)
        for dep in node.depends:
            visit(dep)
        r.append(node)

    for node in g.nodes:
        visit(node)
    return r


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

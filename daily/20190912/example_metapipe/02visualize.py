import sys
import typing as t
from collections import defaultdict
import inspect
import ast


def _get_name(code):
    t = ast.parse(code)
    for node in ast.walk(t):
        if isinstance(node, ast.Assign):
            return node.targets[0].id
    return None


class Node:
    def __init__(
        self,
        name: t.Optional[str] = None,
        id=None,
        *,
        meta: dict = None,
        _level: int = 1,
    ):
        self.src = None
        self.dst = []
        self.meta = meta or {}
        self.id = id
        self.name = name
        if self.name is None:
            stack = inspect.stack(_level)
            self.name = _get_name(stack[_level].code_context[0].strip())

    def __or__(self, dst):
        self.dst.append(dst)
        return dst

    def __repr__(self):
        return f"Node({self.name!r}, dst={self.dst!r})"

    def bind(self, *, meta: dict):
        new = self.__class__(self.name, meta=self.meta.copy())
        new.meta.update(meta)
        return new


def graph(callback, *, out=sys.stdout):
    def cont():
        import inspect

        _locals = inspect.currentframe().f_back.f_locals
        seen = set()
        idgen = defaultdict(lambda: f"g{len(idgen)}")

        nodes = []
        for val in _locals.values():
            if not isinstance(val, Node):
                continue
            if val in seen:
                continue
            seen.add(val)
            nodes.append(val)

        print("digraph {", file=out)
        for node in nodes:
            id = node.id
            if id is None:
                id = node.id = idgen[node]
            print(f'  {node.id} [label="{node.name}", shape="box"];')
        print("")
        for node in nodes:
            for dst in node.dst:
                print(f'  {node.id} -> {dst.id};')

        print("}")

    callback(cont=cont)


def use(*, cont):
    x = Node()
    y = Node()
    z = Node()
    x | y | z
    cont()


graph(use)

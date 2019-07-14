from pycomment.parse import parse_file, node_name, PyTreeVisitor, token
from lib2to3.pytree import Node


class Visitor(PyTreeVisitor):
    def __init__(self, lineno):
        self.lineno = lineno
        self.r = []

    # todo: performance
    def visit(self, node):
        if node.get_lineno() == self.lineno and node.type != token.INDENT:
            self.r.append(node)
            return
        super().visit(node)


def select_node(t: Node, *, lineno: int) -> Node:
    visitor = Visitor(lineno)

    t = parse_file(filename)
    visitor.visit(t)

    return next(iter(visitor.r))  # xxx:


def find_parents(node: Node) -> Node:
    r = [node]
    seen = set()
    target = node

    while target:
        if id(target) in seen:
            break
        if node_name(target) in ("funcdef", "classdef"):
            r.append(target)
        elif target.parent is None:
            break
        seen.add(id(target))
        target = target.parent
    return reversed(r)


def run(filename: str, *, lineno: int) -> None:
    t = parse_file(filename)

    node = select_node(t, lineno=lineno)
    parents = list(find_parents(node))

    for node in parents:
        print(
            f"lineno:{node.get_lineno()}	node:{node_name(node)}	value:{getattr(node.children[1], 'value', '')}"
        )

    print("----------------------------------------")
    import linecache

    for node in parents:
        lineno = node.get_lineno()
        print(f"{lineno:02d}:{linecache.getline(filename, lineno)}", end="")


filename = "target00.py"
run(filename, lineno=4)

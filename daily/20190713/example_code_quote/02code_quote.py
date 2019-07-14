import linecache
from pycomment.parse import parse_string, node_name, PyTreeVisitor, token
from handofcats import as_command
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


@as_command
def run(filename, *, lineno: int, n: int = 2, show_lineno: bool = False):
    t = parse_string("".join(linecache.getlines(filename)))

    seen = []
    targets = list(find_parents(select_node(t, lineno=lineno)))
    for i, node in enumerate(targets):
        target_lineno = node.get_lineno()
        if i == 0:
            start_lineno = target_lineno
        elif i == len(targets) - 1:
            start_lineno = max(1, target_lineno - n)
        else:
            start_lineno = target_lineno

        end_lineno = min(len(linecache.getlines(filename)) + 1, target_lineno + n + 1)

        if seen and start_lineno - seen[-1] > 1:
            if show_lineno:
                print(f"...")
            else:
                print("# ...")

        for lineno in range(start_lineno, end_lineno):
            if seen and lineno <= seen[-1]:
                continue
            seen.append(lineno)
            if show_lineno:
                print(f"{lineno:03d}: {linecache.getline(filename, lineno)}", end="")
            else:
                print(linecache.getline(filename, lineno), end="")

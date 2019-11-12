import typing as t
import inspect
import logging
from lib2to3.pytree import Node
from pycomment.langhelpers import reify
from pycomment.parse import parse_file, PyTreeVisitor, token, node_name


class Visitor(PyTreeVisitor):
    @reify
    def defs(self):
        return {}

    def visit_funcdef(self, node: Node):
        # 'def' <name> ['(' <argument>,* ')'] ':'
        # 'def' <name> ['(' <argument>,* ')'] -> [] ':'
        name = node.children[1].value
        assert node.children[1].type == token.NAME
        self.defs[name] = node
        return False

    def visit_classdef(self, node: Node):
        # 'class' <name>
        name = node.children[1].value
        assert node.children[1].type == token.NAME
        self.defs[name] = node
        return False


# logging.basicConfig(level=logging.DEBUG)
t0 = parse_file("./before.py")
v0 = Visitor()
v0.visit(t0)

t1 = parse_file("./after.py")
v1 = Visitor()
v1.visit(t1)

print(t0)
print("--")


def _parse(node: Node):
    children = node.children
    # [before-colon, ':' after-colon]


for name, node1 in v1.defs.items():
    node0 = v0.defs.get(name)
    if node0 is None:  # insert
        node1.prefix = "\n\n"
        t0.append_child(node1)
    else:  # update
        node0.children = [*node1.children[:-1], node0.children[-1]]
        node0.parent.changed()

print(t0)

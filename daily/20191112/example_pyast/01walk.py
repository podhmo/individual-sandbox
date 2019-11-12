import logging
from lib2to3.pytree import Node
from pycomment.langhelpers import reify
from pycomment.parse import parse_file, PyTreeVisitor


class Visitor(PyTreeVisitor):
    @reify
    def defs(self):
        return {}

    def visit_funcdef(self, node: Node):
        # 'def' <name>
        name = node.children[1].value
        self.defs[name] = node


logging.basicConfig(level=logging.DEBUG)
t0 = parse_file("./before.py")
Visitor().visit(t0)

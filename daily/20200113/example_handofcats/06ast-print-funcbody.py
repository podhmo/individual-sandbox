import inspect
from lib2to3.pytree import Node
from lib2to3.pgen2 import token
import textwrap
from prestring.python.parse import parse_string, PyTreeVisitor
from prestring.python.parse import PyTreeDumper
from handofcats import customize


class FuncBodyPrinter(PyTreeVisitor):
    def visit_funcdef(self, node: Node):
        for i, child in enumerate(node.children):
            if child.type == token.COLON:
                break
        assert len(node.children) == i + 2
        print(textwrap.dedent(str(node.children[i + 1])))


source = inspect.getsource(customize.logging_activate)
t = parse_string(source)

# print(t)
# PyTreeDumper().visit(t)
FuncBodyPrinter().visit(t)

# TODO: args?

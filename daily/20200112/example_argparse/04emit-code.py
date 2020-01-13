import myparser
import inspect
from lib2to3.pytree import Node
from prestring.python.parse import PyTreeVisitor, type_repr, PyTreeDumper
from handofcats.actions._ast import (
    parse_file,
    CollectSymbolVisitor,
    parse_dotted_name,
)
from handofcats.langhelpers import reify


t = parse_file(inspect.getsourcefile(myparser.Both))
v = CollectSymbolVisitor()
v.visit(t)


class V(PyTreeVisitor):
    def visit_classdef(self, node: Node):
        sv = SV()
        sv.visit(node)
        print(sv.names)
        return True


class SV(PyTreeVisitor):
    def __init__(self):
        self.names = []

    def visit_power(self, node: Node):
        # self.names.append(parse_dotted_name(node))
        self.names.append(("@power", str(node).replace(node.prefix, "").strip()))
        return True

    def visit_NAME(self, node: Node):
        # import keyword
        # keyword.iskeyword(node.value)
        self.names.append(("@name", str(node).replace(node.prefix, "").strip()))
        self.names.append(("@name2", node.value))
        return True


print(v.symbols)
V().visit(t)
print(inspect.getsource(myparser.Both))
# PyTreeDumper().visit(t)

from prestring.python.parse import parse_string, PyTreeVisitor
from lib2to3.pytree import Node


class Visitor(PyTreeVisitor):
    def visit_decorator(self, node: Node):
        # AT [ <dotted_name> | Leaf ]
        node.remove()
        return True  # stop


code = """
from handofcats import as_command

@as_command
def hello(name):
    pass
"""

t = parse_string(code)
v = Visitor()
v.visit(t)
print(t)

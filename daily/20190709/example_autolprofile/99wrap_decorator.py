import sys
import logging
from pycomment.parse import parse_string, node_name, PyTreeVisitor
from lib2to3.pgen2 import token
from lib2to3 import fixer_util as u

# todo: with lineno
class Visitor(PyTreeVisitor):
    def __init__(self):
        self.r = []

    def visit_return_stmt(self, node):
        self.r.append(node)


logging.basicConfig(level=logging.DEBUG)


def run(t):
    visitor = Visitor()
    visitor.visit(t)
    for node in visitor.r:
        target = node
        defs = []
        while target:
            if node_name(target) == "funcdef":
                defs.append(target)
            elif target.parent is None:
                break
            target = target.parent
        yield defs


def lineno(x):
    return x.get_lineno()


code = """
@profile
def f(x):
    return x + 1
"""

# DEBUG:pycomment.parse:      visit_decorator (prefix='\n')
# DEBUG:pycomment.parse:        visit_AT (prefix='\n')
# DEBUG:pycomment.parse:        visit_NAME (prefix='')
# DEBUG:pycomment.parse:        visit_NEWLINE (prefix='')

t = parse_string(code)
for defs in run(t):
    for node in defs:
        node.prefix = "@profile\n"
print(t)

print("----------------------------------------", file=sys.stderr)
code = """
def f(x):

    def g(y):
        return y + y
    return g(x + 1)
"""


def Decorator(name):
    return u.Node(278, [u.Leaf(token.AT, "@", prefix=None), u.Name(name), u.Newline()])


def insert_before(node, new_node):
    for i, x in enumerate(node.parent.children):
        if x == node:
            node.parent.insert_child(i, new_node)
            return True
    return False


t = parse_string(code)
for defs in run(t):
    insert_before(defs[0], Decorator("profile"))
    if not defs[0].prefix:
        defs[0].prefix = u.find_indentation(defs[0])
    break
print(t)

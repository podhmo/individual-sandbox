import sys
import logging
from pycomment.parse import parse_string, node_name, PyTreeVisitor
from lib2to3.pgen2 import token
from lib2to3 import fixer_util as u


class Visitor(PyTreeVisitor):
    def __init__(self):
        self.r = []

    def visit_return_stmt(self, node):
        self.r.append(node)


def Decorator(name):
    return u.Node(278, [u.Leaf(token.AT, "@", prefix=None), u.Name(name), u.Newline()])


def insert_before(node, new_node):
    for i, x in enumerate(node.parent.children):
        if x == node:
            node.parent.insert_child(i, new_node)
            return True
    return False


def patch(t):
    visitor = Visitor()
    visitor.visit(t)

    seen = set()
    for node in visitor.r:
        target = node

        while target:
            if node_name(target) == "funcdef":
                if id(target) not in seen:
                    seen.add(id(target))  # xxx:

                    insert_before(target, Decorator("profile"))
                    target.prefix = u.find_indentation(target)
            elif target.parent is None:
                break
            target = target.parent


def run(code):
    print("----------------------------------------")
    print(code)
    t = parse_string(code)
    patch(t)
    print(t)


logging.basicConfig(level=logging.DEBUG)


code = """
def f(x):
    return x + 1
"""
run(code)

code = """
@xxx
def f(x):
    return x + 1
"""
run(code)

code = """
def f(x):
    def g(y):
        return y + y
    return g(x + 1)
"""
run(code)

code = """
def f(x):
    def g(y):
        return y + y

    def h(y):
        return y - y
    return g(h(x) + 1)
"""
run(code)

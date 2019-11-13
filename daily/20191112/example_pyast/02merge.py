import typing as t
import inspect
import logging
from lib2to3.pytree import Node
from pycomment.langhelpers import reify
from pycomment.parse import parse_file, PyTreeVisitor, token, node_name

# TODO: update docstring
# TODO: comment handling
# TODO: support method (insert)


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


def merge(t0, t1):
    """update parameters and docstring by t1, if not existed, inserted"""
    for name, node1 in v1.defs.items():
        node0 = v0.defs.get(name)
        if node0 is None:  # insert
            ancestors = []
            target = node1
            while target is not None:
                ancestors.append(node_name(target))
                target = target.parent

            assert ancestors[-1] == "file_input"
            is_toplevel_def = ancestors[-2] in ("funcdef", "classdef", "async_funcdef")
            if not is_toplevel_def:
                continue
            node1.prefix = "\n\n"
            t0.append_child(node1)
        else:  # update
            suite0 = node0.children[-1]
            assert node_name(suite0) == "suite"
            for i, x in enumerate(suite0.children):
                if node_name(x) == "simple_stmt" and x.children[0].type == token.STRING:
                    break
            has_comment0 = i < (len(suite0.children) - 1)

            suite1 = node1.children[-1]
            assert node_name(suite1) == "suite"
            before_comments1 = []
            for j, x in enumerate(suite1.children):
                before_comments1.append(x)
                if node_name(x) == "simple_stmt" and x.children[0].type == token.STRING:
                    break
            has_comment1 = j < (len(suite1.children) - 1)

            if has_comment1:
                if has_comment0:
                    suite0.children[i] = suite1.children[j]
                else:
                    assert suite0.children[0].type == token.NEWLINE
                    assert suite0.children[1].type == token.INDENT
                    suite0.children = before_comments1 + suite0.children[1:]
                suite0.parent.changed()
            node0.children = [*node1.children[:-1], node0.children[-1]]
            node0.parent.changed()
    return t0


# logging.basicConfig(level=logging.DEBUG)
t0 = parse_file("./before.py")
v0 = Visitor()
v0.visit(t0)

t1 = parse_file("./after.py")
v1 = Visitor()
v1.visit(t1)

print(t0)
print("--")

print(merge(t0, t1))

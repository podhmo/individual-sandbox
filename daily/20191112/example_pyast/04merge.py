from handofcats import as_command
from lib2to3.pytree import Node
from lib2to3.fixer_util import find_indentation
from pycomment.langhelpers import reify
from pycomment.parse import parse_string, PyTreeVisitor, token, node_name


class Visitor(PyTreeVisitor):
    @reify
    def defs(self):
        return {}

    @reify
    def path(self):
        return []

    def visit(self, node):
        try:
            super().visit(node)
            if self.path and node_name(node) in ("funcdef", "classdef"):
                if self.path[-1] == node.children[1].value:
                    self.path.pop()
        finally:
            self.level -= 1

    def visit_funcdef(self, node: Node):
        # 'def' <name> ['(' <argument>,* ')'] ':'
        # 'def' <name> ['(' <argument>,* ')'] -> [] ':'
        name = node.children[1].value
        assert node.children[1].type == token.NAME
        self.path.append(name)
        self.defs[tuple(self.path)] = node
        return False

    def visit_classdef(self, node: Node):
        # 'class' <name>
        name = node.children[1].value
        assert node.children[1].type == token.NAME
        self.path.append(name)
        self.defs[tuple(self.path)] = node
        return False


def replace_code_by_skeleton(base: str, skeleton: str) -> str:
    t0 = parse_string(base)
    t1 = parse_string(skeleton)
    return str(replace_ast_by_skeleton(t0, t1)).rstrip()


def replace_ast_by_skeleton(t0, t1) -> Node:
    """update parameters and docstring by t1, if not existed, inserted"""
    v0 = Visitor()
    v1 = Visitor()

    v0.visit(t0)
    v1.visit(t1)

    for path, node1 in sorted(v1.defs.items(), key=len):
        node0 = v0.defs.get(path)
        if node0 is not None:  # update
            _replace_node(node0, node1)
            continue

        # insert
        if len(path) == 1:
            _insert_node(t0, node1)
            continue
        parent0 = v0.defs.get(path[:-1])
        if parent0 is not None:
            _insert_node(parent0, node1)
    return t0


def _insert_node(t0, node) -> None:
    ancestors = []
    target = node
    while target is not None:
        ancestors.append(node_name(target))
        target = target.parent

    assert ancestors[-1] == "file_input"
    is_toplevel_def = ancestors[-2] in ("funcdef", "classdef", "async_funcdef")
    if not is_toplevel_def:
        return

    indentation = find_indentation(node)
    if indentation == "":
        node.prefix = "\n\n"
    else:
        node.prefix = f"\n{indentation}"
    t0.append_child(node)


def _replace_node(node0: Node, node1: Node) -> None:
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


@as_command
def run(*, left: str, right: str):
    # logging.basicConfig(level=logging.DEBUG)
    with open(left) as rf:
        s0 = rf.read()
    with open(right) as rf:
        s1 = rf.read()
    t0 = replace_code_by_skeleton(s0, s1)
    print(t0)

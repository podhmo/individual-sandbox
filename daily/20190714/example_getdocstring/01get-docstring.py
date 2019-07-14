import typing as t
import inspect
from pyinspect.code.parse import parse_file, PyTreeVisitor, Node, node_name, token
from handofcats import as_command


def get_summary(doc):
    if not doc:
        return doc
    return doc.split("\n\n", 1)[0]


class Visitor(PyTreeVisitor):
    def visit_funcdef(self, node: Node):
        print(f"{get_name(node)}	{get_summary(get_doc(node) or '')}")

    def visit_classdef(self, node: Node):
        print(f"{get_name(node)}	{get_summary(get_doc(node) or '')}")


def get_name(node: Node) -> t.Optional[str]:
    assert node_name(node) in ("funcdef", "classdef", "async_funcdef")
    for x in node.children[1:]:
        if x.type == token.NAME:
            return x.value
    return None


def get_doc(node: Node) -> t.Optional[str]:
    assert node_name(node) in ("funcdef", "classdef", "async_funcdef")
    suite = None
    for x in node.children:
        if node_name(x) == "suite":
            suite = x
            break
    else:
        return None
    for x in suite.children[2:]:
        if node_name(x) == "simple_stmt":
            if x.children[0].type == token.STRING:
                return inspect.cleandoc(x.children[0].value.strip("""\n"'"""))
        return None


@as_command
def run(filename: str) -> None:
    t = parse_file(filename)
    v = Visitor()
    v.visit(t)

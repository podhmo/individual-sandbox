import parselib as p
from lib2to3.pygram import python_symbols as syms
from lib2to3.pgen2 import token


def extract_inner_block(node):
    found = None
    for c in node.children:
        if c.type == syms.suite:
            found = c
            break

    indent_level = 0
    indent = None
    for line in found.children:
        if line.type == token.INDENT:
            indent_level += 1
            indent = line
            continue
        if line.type == token.DEDENT:
            indent_level -= 1
            if indent_level <= 0:
                break
        if indent_level > 0:
            line = line.clone()
            line.prefix = line.prefix.replace(indent.value * indent_level, "")
            yield line


class Visitor(p.StrictPyTreeVisitor):
    def visit_file_input(self, node):
        # iterate only toplevel
        for c in node.children:
            self.visit(c)

    def visit_simple_stmt(self, node):
        print("stmt", node)

    def visit_decorated(self, node):
        print("decorated", node)

    def visit_with_stmt(self, node):
        if node.children[1].children[0].value == "code":
            for line in extract_inner_block(node):
                print("stmt", line)
                # self.visit(line)
        else:
            print("with", node)

    def visit_ENDMARKER(self, node):
        pass


code = """
with code():
    print("hello")
    if True:
        print("hoi")
    print("hai")
"""

t = p.parse_from_string(code)
visitor = Visitor()
visitor.visit(t)

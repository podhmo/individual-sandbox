from lib2to3 import pytree
from lib2to3 import pygram
from lib2to3.pgen2 import driver
from lib2to3.pgen2 import token

default_driver = driver.Driver(
    pygram.python_grammar_no_print_statement, convert=pytree.convert
)


def parse_string(code, parser_driver=default_driver, *, debug=True):
    return parser_driver.parse_string(code, debug=debug)


def node_name(node):
    # Nodes with values < 256 are tokens. Values >= 256 are grammar symbols.
    if node.type < 256:
        return token.tok_name[node.type]
    else:
        return pygram.python_grammar.number2symbol[node.type]


def node_value(node):
    if node_name(node) == "NAME":
        return repr(node)
    else:
        return "..."


class PyTreeVisitor:
    def visit(self, node):
        method = "visit_{0}".format(node_name(node))
        if hasattr(self, method):
            # Found a specific visitor for this node
            if getattr(self, method)(node):
                return

        elif hasattr(node, "value"):  # Leaf
            self.default_leaf_visit(node)
        else:
            self.default_node_visit(node)

    def default_node_visit(self, node):
        for child in node.children:
            self.visit(child)

    def default_leaf_visit(self, leaf):
        pass

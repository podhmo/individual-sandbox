import inspect
from lib2to3.pytree import Node
from lib2to3.pgen2 import token
import textwrap
from prestring.python import Module
from prestring.python.parse import parse_string, PyTreeVisitor
from prestring.python.parse import PyTreeDumper
from prestring.python.transform import transform
from handofcats import customize

candidates = []


class FuncBodyCollector(PyTreeVisitor):
    def visit_funcdef(self, node: Node):
        assert node.children[0].value == "def"
        name = node.children[1].value

        for i, child in enumerate(node.children):
            if child.type == token.COLON:
                break
        assert len(node.children) == i + 2

        candidates.append(f"# begin: {name}")
        candidates.append(textwrap.dedent(str(node.children[i + 1])))
        candidates.append(f"# end: {name}")


source = inspect.getsource(customize.logging_activate)
t = parse_string(source)

# print(t)
# PyTreeDumper().visit(t)
FuncBodyCollector().visit(t)

m = Module()
with m.def_("main"):
    code = str(transform("\n".join(candidates), indent="    "))
    # xxx:
    exec(code, globals(), {"m": m})
print(m)
# TODO: args?

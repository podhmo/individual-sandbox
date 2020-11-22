import ast
import sys


class foo:
    class Foo:
        pass

    class Bar:
        pass


class Transformer(ast.NodeTransformer):
    # def visit(self, node):
    #     method = "visit_" + node.__class__.__name__
    #     visitor = getattr(self, method, self.generic_visit)
    #     print("@", method)
    #     return visitor(node)

    def visit_Import(self, node: ast.Import) -> ast.stmt:
        # Import(names=[alias(name='foo', asname=None)])
        # Import(names=[alias(name='foo', asname='foo2')])
        # Import(names=[alias(name='foo', asname=None), alias(name='bar', asname=None)])
        code = []
        for name in node.names:
            if name.asname is None:
                code.append(f"{name.name}")
            else:
                code.append(f"{name.name} as {name.asname}")
        t: ast.Module = ast.parse(f'print("# import", {", ".join(code)!r})')
        assert len(t.body) == 1
        return t.body[0]

    def visit_ImportFrom(self, node: ast.ImportFrom) -> ast.stmt:
        # ImportFrom(module='foo', names=[alias(name='Foo', asname=None)], level=0)
        # ImportFrom(module='foo', names=[alias(name='Foo', asname=None), alias(name='Bar', asname=None), alias(name='Foo', asname='Foo2')], level=0)
        code = []
        for name in node.names:
            if name.asname is None:
                code.append(f"{name.name}")
            else:
                code.append(f"{name.name} as {name.asname}")
        t: ast.Module = ast.parse(
            f'print("# from {node.module} import", {", ".join(code)!r})'
        )
        assert len(t.body) == 1
        return t.body[0]


sys.modules["foo"] = foo
sys.modules["bar"] = foo
code = """
import foo
import foo as foo2
import foo, bar
from foo import Foo
from foo import Foo, Bar, Foo as Foo2
from foo import (
 Foo, Bar,
 Foo as Foo2,
)
"""
t = ast.parse(code)
Transformer().visit(t)
# ast.fix_missing_locations(t)
print("----------------------------------------")
exec(compile(t, filename="<ast>", mode="exec"))
print("----------------------------------------")

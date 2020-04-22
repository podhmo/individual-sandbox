import inspect
import ast


class Transformer(ast.NodeTransformer):
    def visit(self, node: "ast.AST") -> "ast.AST":
        print("@", "visit_" + node.__class__.__name__)
        super().visit(node)
        return node


def hello(*, name: str) -> None:
    """hello message"""
    from gengo import runtime

    runtime.printf("Hello %s \n", name)


tree = ast.parse(inspect.getsource(hello))
transformer = Transformer()
transformer.visit(tree)
print(tree)

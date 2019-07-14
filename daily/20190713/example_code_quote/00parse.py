from pycomment.parse import parse_file, node_name, PyTreeVisitor, token


class Visitor(PyTreeVisitor):
    def __init__(self, lineno):
        self.lineno = lineno
        self.r = []

    # todo: performance
    def visit(self, node):
        if node.get_lineno() == self.lineno and node.type != token.INDENT:
            self.r.append(node)
            return
        super().visit(node)


def run(filename: str, *, lineno: int) -> None:
    visitor = Visitor(lineno)

    t = parse_file(filename)
    visitor.visit(t)

    node = next(iter(visitor.r))
    print(f"lineno:{node.get_lineno()}	code:{str(node).rstrip()}	node:{node!r}")


filename = "target00.py"
run(filename, lineno=4)

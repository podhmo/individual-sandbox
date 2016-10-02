import ast
import codegen
from collections import OrderedDict


# monkey patching for support python3
def visit_arg(self, node):
    self.write(node.arg)
codegen.SourceGenerator.visit_arg = visit_arg


def update_funs(t0, t1):
    d = OrderedDict()
    used = set()

    class FunDefScanner(ast.NodeVisitor):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self.stack = []

        def visit_FunctionDef(self, node):
            if not self.stack:
                # collecting top level function only
                d[node.name] = node
            self.stack.append(True)
            super().generic_visit(node)
            self.stack.pop()

    visitor = FunDefScanner()
    visitor.visit(t1)

    class FunDefMerger(ast.NodeVisitor):
        def visit_FunctionDef(self, node):
            if node.name in d:
                node.args = d[node.name].args
                ast.fix_missing_locations(node.args)
                used.add(node.name)
    visitor = FunDefMerger()
    visitor.visit(t0)

    for name, node in d.items():
        if name not in used:
            t0.body.append(node)
            ast.fix_missing_locations(t0.body[-1])
    return codegen.to_source(t0)


def main():
    with open("./funs.py") as rf:
        t0 = ast.parse(rf.read(), rf.name)

    with open("./newfuns.py") as rf:
        t1 = ast.parse(rf.read(), rf.name)

    print(update_funs(t0, t1))

if __name__ == "__main__":
    main()

import typing as t
import ast
import logging
from handofcats import as_command

logger = logging.getLogger(__name__)


class StrictVisitor(ast.NodeVisitor):
    def __init__(self, env: t.Optional[t.Dict[str, t.Any]] = None) -> None:
        self.stack = [[]]
        self.env = env or {}

    # override
    def generic_visit(self, node: ast.AST):
        method = "visit_" + node.__class__.__name__
        raise NotImplementedError(method)

    def visit_Module(self, node: ast.Module):
        logger.debug("trace Module depth=%d %s", len(self.stack), node)
        assert len(node.body) == 1, "must be expr, len(node) == 1"
        self.visit(node.body[0])

    def visit_Expr(self, node: ast.Expr):
        logger.debug("trace Expr depth=%d %s", len(self.stack), node)
        self.visit(node.value)

    def visit_Name(self, node: ast.Name):
        logger.debug("trace Name depth=%d %s", len(self.stack), node)
        self.stack[-1].append(self.env[node.id])

    def visit_Constant(self, node: ast.Constant):
        logger.debug("trace Constant depth=%d %s", len(self.stack), node)
        self.stack[-1].append(ast.literal_eval(node))

    def visit_Compare(self, node: ast.Compare):
        # short circuit?
        logger.debug(
            "trace Compare depth=%d %s, %s, %s",
            len(self.stack),
            node,
            node.ops,
            node.comparators,
        )
        self.stack.append([])
        self.visit(node.left)
        l_val = self.stack[-1].pop()

        assert len(node.ops) == len(node.comparators)
        result = []
        for i, (op, right) in enumerate(zip(node.ops, node.comparators)):
            self.visit(right)
            r_val = self.stack[-1].pop()

            if isinstance(op, ast.Gt):
                result.append(l_val > r_val)
            elif isinstance(op, ast.GtE):
                result.append(l_val >= r_val)
            elif isinstance(op, ast.Lt):
                result.append(l_val < r_val)
            elif isinstance(op, ast.LtE):
                result.append(l_val <= r_val)
            elif isinstance(op, ast.Eq):
                result.append(l_val == r_val)
            elif isinstance(op, ast.NotEq):
                result.append(l_val != r_val)
            elif isinstance(op, ast.Is):
                result.append(l_val is r_val)
            elif isinstance(op, ast.IsNot):
                result.append(l_val is not r_val)
            else:
                raise NotImplementedError(f"{op}")
            l_val = r_val

        assert not self.stack.pop()  # empty list
        self.stack[-1].append(all(result))


def run(code: str, *, env: t.Optional[t.Dict[str, t.Any]] = None):
    print("----------------------------------------")
    print(f"input : {code}")
    t = ast.parse(code)
    v = StrictVisitor(env=env)
    v.visit(t)
    print(f"output: {v.stack}")


@as_command
def main():
    # Name
    run("x")
    # Compare
    run("x > 10", env={"x": 10})

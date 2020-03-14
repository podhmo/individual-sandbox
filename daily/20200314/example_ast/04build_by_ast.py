import ast
import logging
from handofcats import as_command
from q import q

logger = logging.getLogger(__name__)


class StrictVisitor(ast.NodeVisitor):
    def __init__(self) -> None:
        self.stack = [[]]

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
        self.stack[-1].append(q(node.id))

    def visit_Constant(self, node: ast.Constant):
        logger.debug("trace Constant depth=%d %s", len(self.stack), node)
        self.stack[-1].append(q(repr(ast.literal_eval(node))))

    def visit_BinOp(self, node: ast.BinOp):
        logger.debug(
            "trace BinOp depth=%d %s, %s, %s",
            len(self.stack),
            node,
            node.left,
            node.right,
        )
        self.stack.append([])
        self.visit(node.left)
        self.visit(node.right)
        left, right = self.stack.pop()

        method = getattr(left, node.op.__class__.__name__)
        self.stack[-1].append(method(right))

    def visit_BoolOp(self, node: ast.BoolOp):
        # short circuit?
        logger.debug(
            "trace BoolOp depth=%d %s, %s, %s",
            len(self.stack),
            node,
            node.op,
            node.values,
        )
        self.stack.append([])
        self.visit(node.values[0])
        l_value = self.stack[-1].pop()

        for v in node.values[1:]:
            self.visit(v)
            method = getattr(l_value, node.op.__class__.__name__)
            r_value = self.stack[-1].pop()
            l_value = method(r_value)

        assert not self.stack.pop()  # empty list
        self.stack[-1].append(l_value)

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
        for i, (op, right) in enumerate(zip(node.ops, node.comparators)):
            self.visit(right)
            r_val = self.stack[-1].pop()
            method = getattr(l_val, op.__class__.__name__)
            l_val = method(r_val)

        assert not self.stack.pop()  # empty list
        self.stack[-1].append(l_val)

    def visit_Subscript(self, node: ast.Subscript):
        logger.debug(
            "trace Subscript depth=%d %s, %s, %s",
            len(self.stack),
            node,
            node.value,
            node.slice,
        )

        # e.g. d["x"]
        self.stack.append([])
        self.visit(node.value)
        c = self.stack[-1].pop()
        self.visit(node.slice.value)
        k = self.stack[-1].pop()

        assert not self.stack.pop()
        self.stack[-1].append(c.__getitem__(k))

    def visit_Attribute(self, node: ast.Attribute):
        logger.debug(
            "trace Attribute depth=%d %s, %s, %s",
            len(self.stack),
            node,
            node.value,
            node.attr,
        )

        # e.g. ob.x
        self.stack.append([])
        self.visit(node.value)
        ob = self.stack[-1].pop()

        assert not self.stack.pop()
        self.stack[-1].append(getattr(ob, node.attr))

    def visit_Call(self, node: ast.Call):
        logger.debug(
            "trace Call depth=%d %s, %s, %s, %s",
            len(self.stack),
            node,
            node.func,
            node.args,
            node.keywords,
        )

        self.stack.append([])
        self.visit(node.func)
        fn = self.stack[-1].pop()

        for x in node.args:
            self.visit(x)
        args = self.stack.pop()

        self.stack.append([])
        for keyword in node.keywords:
            # keyword
            self.visit(keyword.value)
            v = self.stack[-1].pop()
            self.stack[-1].append((keyword.arg, v))
        kwargs = dict(self.stack.pop())

        self.stack[-1].append(fn(*args, **kwargs))


def run(code: str, *, env=None):
    print("----------------------------------------")
    print(f"input : {code}")
    t = ast.parse(code)
    v = StrictVisitor()
    v.visit(t)
    print(f"output: {v.stack[-1][-1]}")


@as_command
def main():
    # run("1")
    # # BinOp
    # run("1 + 1")
    # run("2 * 3")
    # run("2 * (3 + 1)")
    # run("(2 * 3) + 1")
    # # Name
    # run("x", env={"x": 10})
    # run("x + 1", env={"x": 10})
    # run("(x + 1) * x", env={"x": 10})
    # run("x * 3", env={"x": "foo"})
    # # Compare
    # run("x > 10", env={"x": 10})
    # run("x >= 10", env={"x": 10})
    # run("0 < x <= 10", env={"x": 10})
    # run("0 < x < 10", env={"x": 10})
    # run("10 < x <= 100", env={"x": 10})
    # # BoolOp
    # run("True and True")
    # run("True and False")
    # run("False or True")
    # run("False or False")
    # run("0 < x and x <= 10", env={"x": 10})
    # # dict
    # run("d['x'] + d['y']", env={"d": {"x": 10, "y": 20}})
    # object
    class ob:
        x = 10
        y = 20

    run("ob.x * ob.y", env={"ob": ob})

    # # call
    # run("x.center(10)", env={"x": "foo"})
    # run("x.split(sep='/')", env={"x": "foo/bar/boo"})

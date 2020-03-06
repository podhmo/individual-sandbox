import typing as t
import ast
from handofcats import as_command, debug


class StrictVisitor(ast.NodeVisitor):
    def __init__(self, env: t.Optional[t.Dict[str, t.Any]] = None) -> None:
        self.stack = [[]]
        self.env = env or {}

    # override
    def generic_visit(self, node: ast.AST):
        method = "visit_" + node.__class__.__name__
        raise NotImplementedError(method)

    def visit_Module(self, node: ast.Module):
        debug(f"""trace: {"   " * len(self.stack)} ({_repr(node)})""")
        assert len(node.body) == 1, "must be expr, len(node) == 1"
        self.visit(node.body[0])

    def visit_Expr(self, node: ast.Expr):
        debug(f"""trace: {"   " * len(self.stack)} ({_repr(node)})""")
        self.visit(node.value)

    def visit_Name(self, node: ast.Name):
        debug(f"""trace: {"   " * len(self.stack)} ({_repr(node)})""")
        self.stack[-1].append(self.env[node.id])

    def visit_Constant(self, node: ast.Constant):
        debug(f"""trace: {"   " * len(self.stack)} ({_repr(node)})""")
        self.stack[-1].append(ast.literal_eval(node))

    def visit_BinOp(self, node: ast.BinOp):
        debug(
            f"""trace: {"   " * len(self.stack)} ({_repr(node)} {_repr(node.left)} {_repr(node.right)})"""
        )
        self.stack.append([])
        self.visit(node.left)
        self.visit(node.right)
        left, right = self.stack.pop()

        if isinstance(node.op, ast.Add):
            self.stack[-1].append(left + right)
        elif isinstance(node.op, ast.Mult):
            self.stack[-1].append(left * right)
        else:
            raise NotImplementedError(f"{node.op}")

    def visit_BoolOp(self, node: ast.BoolOp):
        # short circuit?
        debug(
            f"""trace: {"   " * len(self.stack)} ({_repr(node)} {_repr(node.op)} {[_repr(v) for v in node.values]})"""
        )
        self.stack.append([])
        self.visit(node.values[0])
        l_value = self.stack[-1].pop()

        if isinstance(node.op, ast.And):
            op = lambda x, y: x and y
        elif isinstance(node.op, ast.Or):
            op = lambda x, y: x or y
        else:
            raise NotImplementedError(f"{node.op}")

        for v in node.values[1:]:
            self.visit(v)
            r_value = self.stack[-1].pop()
            l_value = op(l_value, r_value)

        assert not self.stack.pop()  # empty list
        self.stack[-1].append(l_value)

    def visit_Compare(self, node: ast.Compare):
        # short circuit?

        debug(
            f"""trace: {"   " * len(self.stack)} ({_repr(node)} {[_repr(x) for x in node.ops]} {[_repr(x) for x in node.comparators]})"""
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

    def visit_Subscript(self, node: ast.Subscript):
        debug(
            f"""trace: {"   " * len(self.stack)} ({_repr(node)} {_repr(node.value)} {_repr(node.slice)})"""
        )

        # e.g. d["x"]
        self.stack.append([])
        self.visit(node.value)
        c = self.stack[-1].pop()
        self.visit(node.slice.value)
        k = self.stack[-1].pop()

        assert not self.stack.pop()
        self.stack[-1].append(c[k])

    def visit_Attribute(self, node: ast.Attribute):
        debug(
            f"""trace: {"   " * len(self.stack)} ({_repr(node)} {_repr(node.value)} {_repr(node.attr)})"""
        )

        # e.g. ob.x
        self.stack.append([])
        self.visit(node.value)
        ob = self.stack[-1].pop()

        assert not self.stack.pop()
        self.stack[-1].append(getattr(ob, node.attr))

    def visit_Call(self, node: ast.Call):
        debug(
            f"""trace: {"   " * len(self.stack)} ({_repr(node)} {_repr(node.func)} {_repr(node.args)} {_repr(node.keywords)})"""
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


def _repr(node: ast.AST):
    return node.__class__.__name__


def run(code: str, *, env: t.Optional[t.Dict[str, t.Any]] = None):
    print("----------------------------------------")
    print(f"input : {code}")
    t = ast.parse(code)
    v = StrictVisitor(env=env)
    v.visit(t)
    print(f"output: {v.stack}")


@as_command
def main():
    run("1")
    # BinOp
    run("1 + 1")
    run("2 * 3")
    run("2 * (3 + 1)")
    run("(2 * 3) + 1")
    # Name
    run("x", env={"x": 10})
    run("x + 1", env={"x": 10})
    run("(x + 1) * x", env={"x": 10})
    run("x * 3", env={"x": "foo"})
    # Compare
    run("x > 10", env={"x": 10})
    run("x >= 10", env={"x": 10})
    run("0 < x <= 10", env={"x": 10})
    run("0 < x < 10", env={"x": 10})
    run("10 < x <= 100", env={"x": 10})
    # BoolOp
    run("True and True")
    run("True and False")
    run("False or True")
    run("False or False")
    run("0 < x and x <= 10", env={"x": 10})
    # dict
    run("d['x'] + d['y']", env={"d": {"x": 10, "y": 20}})

    # object
    class ob:
        x = 10
        y = 20

    run("ob.x * ob.y", env={"ob": ob})

    # call
    run("x.center(10)", env={"x": "foo"})
    run("x.split(sep='/')", env={"x": "foo/bar/boo"})

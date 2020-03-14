import ast
from functools import partial
from handofcats import as_command
from q import QEvaluator, QBuilder, q, Q, QArgs
from visitor import StrictVisitor


class ContextForBuilding:
    def __init__(self, env):
        self.env = env
        self.builder = QBuilder()

    def Name(self, name: str) -> Q:
        return q(name, builder=self.builder)

    def Value(self, value: object) -> Q:
        return q(repr(value), builder=self.builder)

    def Tuple(self, xs) -> Q:
        return q("({args})", args=QArgs(self.builder, xs, {}))

    def List(self, xs) -> Q:
        return q("[{args}]", args=QArgs(self.builder, xs, {}))

    def Set(self, xs) -> Q:
        return q("{{{args}}}", args=QArgs(self.builder, xs, {}))

    def Dict(self, ks, vs) -> Q:
        return q(
            "{{{args}}}", args=QArgs(self.builder, [], dict(zip(ks, vs)), sep=": ")
        )


class ContextForEvaluation:
    def __init__(self, env):
        self.env = env
        self.builder = QEvaluator()

    def Name(self, name: str) -> Q:
        return q(self.env[name], builder=self.builder)

    def Value(self, value: object) -> Q:
        return q(value, builder=self.builder)

    def Tuple(self, xs) -> Q:
        return q(tuple(getattr(x, "val", x) for x in xs))

    def List(self, xs) -> Q:
        return q([getattr(x, "val", x) for x in xs])

    def Set(self, xs) -> Q:
        return q(set(getattr(x, "val", x) for x in xs))

    def Dict(self, ks, vs) -> Q:
        return q({getattr(k, "val", k): getattr(v, "val", v) for k, v in zip(ks, vs)})


def run(code: str, *, create_ctx, env=None):
    print("----------------------------------------")
    print(f"env   : {env}")
    print(f"input : {code}")
    t = ast.parse(code)
    v = StrictVisitor(create_ctx(env))
    v.visit(t)
    print(f"output: {v.stack[-1][-1]}")


@as_command
def main():
    def do_all(_run):
        _run("1")
        # BinOp
        _run("1 + 1")
        _run("2 * 3")
        _run("2 * (3 + 1)")
        _run("(2 * 3) + 1")
        # Name
        _run("x", env={"x": 10})
        _run("x + 1", env={"x": 10})
        _run("(x + 1) * x", env={"x": 10})
        _run("x * 3", env={"x": "foo"})
        # Compare
        _run("x > 10", env={"x": 10})
        _run("x >= 10", env={"x": 10})
        _run("0 < x <= 10", env={"x": 10})
        _run("0 < x < 10", env={"x": 10})
        _run("10 < x <= 100", env={"x": 10})
        _run("0 < x <= 10 < y < 20", env={"x": 10, "y": 20})
        _run("0 < x <= 10 < y <= 20", env={"x": 10, "y": 20})
        # BoolOp
        _run("True and True")
        _run("True and False")
        _run("False or True")
        _run("False or False")
        _run("0 < x and x <= 10", env={"x": 10})
        # dict
        _run("d['x'] + d['y']", env={"d": {"x": 10, "y": 20}})
        # object
        class ob:
            x = 10
            y = 20

        _run("ob.x * ob.y", env={"ob": ob})

        # call
        _run("x.center(10)", env={"x": "foo"})
        _run("x.split(sep='/')", env={"x": "foo/bar/boo"})

        # tuple, list, dict, set
        _run("""(d["x"], d.get("y"))""", env={"d": {"x": 10}})
        _run("""[d["x"], d.get("y")]""", env={"d": {"x": 10}})
        _run("""{"x": d["x"], "y": d.get("y")}""", env={"d": {"x": 10}})
        _run("""{1, x, 3}""", env={"x": 10})

    do_all(partial(run, create_ctx=ContextForBuilding))
    print("")
    print("****")
    print("")
    do_all(partial(run, create_ctx=ContextForEvaluation))

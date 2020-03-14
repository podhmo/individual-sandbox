import ast
from functools import partial
from handofcats import as_command
from q import q, QEvaluator, Q
from visitor import StrictVisitor


class MQEvaluator(QEvaluator):
    def getindex(self, q, name):
        ob = getattr(q, "val", q)
        if ob is None:
            return q
        if not hasattr(ob, "__getitem__"):
            zero = getattr(self.__class__, "zero", None)
            if zero is None:
                zero = self.__class__.zero = q.__class__(self, None, kwargs=None)
            return zero
        return q.__class__(self, ob.get(getattr(name, "val", name)), kwargs=None)


class ContextForMEvaluation:
    def __init__(self, env):
        self.env = env
        self.builder = MQEvaluator()

    def Name(self, name: str) -> Q:
        return q(self.env[name], builder=self.builder)

    def Value(self, value: object) -> Q:
        return q(value, builder=self.builder)

    def Tuple(self, xs) -> Q:
        return q(tuple(getattr(x, "val", x) for x in xs))

    def List(self, xs) -> Q:
        return q([getattr(x, "val", x) for x in xs])

    def Dict(self, ks, vs) -> Q:
        return q({getattr(k, "val", k): getattr(v, "val", v) for k, v in zip(ks, vs)})

    def Set(self, xs) -> Q:
        return q(set(getattr(x, "val", x) for x in xs))


# print(q({"x": 1})["x"]["y"]["z"])
# print(q({})[q("x")]["y"]["z"])

# q = partial(q, builder=MQEvaluator())
# print(q({"x": 1})["x"])
# print(q({"x": 1})["x"]["y"])
# print(q({"x": 1})["x"]["y"]["z"])
# print(q({"x": {"y": {"z": 1}}})["x"]["y"]["z"])


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
    _run = partial(run, create_ctx=ContextForMEvaluation)
    _run("1")
    _run("x", env={"x": 10})
    _run("""d["x"]""", env={"d": {"x": 10}})
    _run("""(d["x"], d["y"])""", env={"d": {"x": 10}})
    _run("""[d["x"], d["y"]]""", env={"d": {"x": 10}})
    _run("""[d["x"], d["y"], d["x"]["y"]["z"]]""", env={"d": {"x": 10}})
    _run("""{"x": d["x"], "y": d["y"]}""", env={"d": {"x": 10}})

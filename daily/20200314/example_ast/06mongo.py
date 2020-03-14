import typing as t
import ast
from functools import partial
from handofcats import as_command
from q import q, QBuilder, Q, QArgs
from visitor import StrictVisitor


class QMongoBuilder(QBuilder):
    bop_mapping = {
        "==": "$eq",
        ">=": "$gte",
        ">": "$gt",
        "<=": "$lte",
        "<": "$lt",
        "in": "$in",
        "and": "$and",
        "or": "$or",
    }

    def bop(self, q, name, right):
        name = self.bop_mapping.get(name, name)
        if name == "$and" or name == "$or":
            fmt = '{{"{op}": [{left}, {right}]}}'
        else:
            fmt = '{{"{left}": {{"{op}": {right}}}}}'
        return q.__class__(self, fmt, kwargs=dict(op=name, left=q, right=right))


class ContextForBuilding:
    def __init__(self, env):
        self.env = env
        self.builder = QMongoBuilder()

    def create_from_name(self, node: ast.Name) -> Q:
        return q(node.id, builder=self.builder)

    def create_from_value(self, node: ast.Constant) -> Q:
        return q(repr(ast.literal_eval(node)), builder=self.builder)

    def List(self, xs: t.List[Q]) -> Q:
        return q("[{args}]", args=QArgs(self.builder, xs, {}))


def run(code: str, *, create_ctx, env=None):
    print("----------------------------------------")
    print(f"input : {code}")
    t = ast.parse(code)
    v = StrictVisitor(create_ctx(env))
    v.visit(t)
    print(f"output: {v.stack[-1][-1]}")


@as_command
def main():
    _run = partial(run, create_ctx=ContextForBuilding)
    _run("1")
    _run("x")
    _run('name == "foo"')
    _run("age >= 20")
    _run('name == "foo" and age >= 20')
    _run('(name == "foo" or name == "boo") and age >= 20')
    _run('name == "foo" or (name == "boo" and age >= 20)')
    _run('name == "foo" or name == "boo" and age >= 20')
    _run("name in [1, 2, 3, 4, 5]")

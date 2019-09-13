import textwrap
import io
import sys
import inspect
import ast
import typing as t
from collections import defaultdict


def _get_name(code):
    t = ast.parse(code)
    for node in ast.walk(t):
        if isinstance(node, ast.Assign):
            return node.targets[0].id
    return None


class Node:
    def __init__(
        self,
        name: t.Optional[str] = None,
        id=None,
        *,
        meta: dict = None,
        _level: int = 1,
    ):
        self.src = None
        self.dst = []
        self.meta = meta or {}
        self.id = id
        self.name = name
        if self.name is None:
            stack = inspect.stack(_level)
            self.name = _get_name(stack[1].code_context[0].strip())

    def __or__(self, dst):
        self.dst.append(dst)
        return dst

    def __repr__(self):
        return f"Node({self.name!r}, dst={self.dst!r})"

    def bind(self, *, meta: dict):
        new = self.__class__(self.name, meta=self.meta.copy())
        new.meta.update(meta)
        return new


def graphviz(callback, *, out=sys.stdout):
    def cont():
        import inspect

        _locals = inspect.currentframe().f_back.f_locals
        seen = set()
        idgen = defaultdict(lambda: f"g{len(idgen)}")

        nodes = []
        for val in _locals.values():
            if not isinstance(val, Node):
                continue
            if val in seen:
                continue
            seen.add(val)
            nodes.append(val)

        print("digraph {", file=out)
        for node in nodes:
            if node.id is None:
                node.id = idgen[node]
            print(f'  {node.id} [label="{node.name}", shape="box"];')
        print("")
        for node in nodes:
            for dst in node.dst:
                print(f"  {node.id} -> {dst.id};")

        print("}")

    callback(cont=cont)


def code_compile(rf, *, filename=None):
    filename = filename or rf.name
    o = io.StringIO()
    indent = "    "
    print("def callback(*, cont):", file=o)
    lines = textwrap.dedent(rf.read().strip()).split("\n")
    for line in lines[1:]:
        print(f"{indent}{line}", file=o)
    print(f"{indent}cont()", file=o)

    env = {}
    env.update(globals())  # xxx
    code = compile(o.getvalue(), filename, mode="exec")
    exec(code, env)
    callback = env["callback"]
    return callback


def main():
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("filenames", nargs="+")
    args = parser.parse_args()
    for filename in args.filenames:
        with open(filename) as rf:
            ac = code_compile(rf)
            graphviz(ac)


if __name__ == "__main__":
    main()

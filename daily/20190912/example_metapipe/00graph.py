import typing as t
import inspect
import ast


def _get_name(code):
    t = ast.parse(code)
    for node in ast.walk(t):
        if isinstance(node, ast.Assign):
            return node.targets[0].id
    return None


class Node:
    def __init__(
        self, name: t.Optional[str] = None, *, meta: dict = None, _level: int = 1
    ):
        self.src = None
        self.dst = []
        self.meta = meta or {}
        self.name = name
        if self.name is None:
            stack = inspect.stack(_level)
            self.name = _get_name(stack[_level].code_context[0].strip())

    def __or__(self, dst):
        self.dst.append(dst)
        return dst

    def __repr__(self):
        return f"Node({self.name!r}, dst={self.dst!r})"

    def bind(self, *, meta: dict):
        new = self.__class__(self.name, meta=self.meta.copy())
        new.meta.update(meta)
        return new


x = Node()
y = Node()
z = Node()
x | y.bind(meta=dict(fifo=True)) | z


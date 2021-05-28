from typing import Callable, Optional, Type

from mypy import nodes
from mypy.plugin import ClassDefContext, Plugin


def plugin(version: str) -> Type[Plugin]:
    return MyPlugin


class MyPlugin(Plugin):
    def get_base_class_hook(
        self, fullname: str
    ) -> Optional[Callable[[ClassDefContext], None]]:
        if fullname == "04genclass.Base":
            return dynamic_init_hook__literal
        if fullname == "gen.Base":
            return dynamic_init_hook
        return None


# TODO: speed-up


def dynamic_init_hook__literal(ctx: ClassDefContext) -> None:
    if "__init__" in ctx.cls.info.names:
        return

    import mypy.fastparse

    code = """\
def __init__(self, name:str) -> None:
    ...
"""
    ff = mypy.fastparse.parse(code, "", "x")
    node = ff.defs[0]
    ctx.api.visit_func_def(node)
    ctx.cls.info.names["__init__"] = nodes.SymbolTableNode(
        nodes.MDEF, node, plugin_generated=True
    )


# https://github.com/Python/mypy/blob/master/mypy/plugins/dataclasses.py
# https://github.com/Python/mypy/blob/master/mypy/plugins/attrs.py
def dynamic_init_hook(ctx: ClassDefContext) -> None:
    if "__init__" in ctx.cls.info.names:
        return

    # transform
    from mypy.plugins.common import add_method_to_class
    from mypy.types import NoneType
    from mypy.nodes import (
        AssignmentStmt,
        NameExpr,
        PlaceholderNode,
        Var,
        Argument,
        ARG_NAMED,
    )

    # from: mypy.plugins.dataclasses
    args = []
    cls = ctx.cls
    for stmt in cls.defs.body:
        # Any assignment that doesn't use the new type declaration
        # syntax can be ignored out of hand.
        if not (isinstance(stmt, AssignmentStmt) and stmt.new_syntax):
            continue

        # a: int, b: str = 1, 'foo' is not supported syntax so we
        # don't have to worry about it.
        lhs = stmt.lvalues[0]
        if not isinstance(lhs, NameExpr):
            continue

        sym = cls.info.names.get(lhs.name)
        if sym is None:
            # This name is likely blocked by a star import. We don't need to defer because
            # defer() is already called by mark_incomplete().
            continue

        node = sym.node
        if isinstance(node, PlaceholderNode):
            # This node is not ready yet.
            return None
        assert isinstance(node, Var)

        # x: ClassVar[int] is ignored by dataclasses.
        if node.is_classvar:
            continue

        args.append(Argument(node, node.type, None, ARG_NAMED))

    add_method_to_class(ctx.api, cls, "__init__", args=args, return_type=NoneType())

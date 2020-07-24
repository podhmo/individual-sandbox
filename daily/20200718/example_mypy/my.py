from typing import Optional, Callable
from mypy.plugin import Plugin, DynamicClassDefContext
from mypy.nodes import (
    ClassDef,
    Block,
    TypeInfo,
    SymbolTable,
    SymbolTableNode,
    GDEF,
    Var,
)


class CustomPlugin(Plugin):
    def get_dynamic_class_hook(
        self, fullname: str
    ) -> Optional[Callable[[DynamicClassDefContext], None]]:
        if fullname == "named.named":
            print("@", fullname)
            return named_hook
        return None


# DynamicClassDefContext(call=<mypy.nodes.CallExpr object at 0x10eef1ba0>, name='XXX', api=<mypy.semanal.SemanticAnalyzer object at 0x10ec5fdc0>)
def named_hook(ctx: DynamicClassDefContext) -> None:
    breakpoint()
    info = TypeInfo(SymbolTable(), ctx.call.args[1], ctx.api.cur_mod_id)
    ctx.call.args[1].info = info
    obj = ctx.api.builtin_type("builtins.object")
    info.mro = [info, obj.type]
    info.bases = [obj]

    ctx.api.add_symbol_table_node(ctx.name, SymbolTableNode(GDEF, info))
    print("hoi")
    return


# def named_hook(ctx: DynamicClassDefContext) -> None:
#     # breakpoint()
#     class_def = ClassDef(ctx.name, Block([]))
#     class_def.fullname = ctx.api.qualified_name(ctx.name)

#     info = TypeInfo(SymbolTable(), class_def, ctx.api.cur_mod_id)
#     class_def.info = info
#     obj = ctx.api.builtin_type("builtins.object")
#     info.mro = [info, obj.type]
#     info.bases = [obj]

#     ctx.api.add_symbol_table_node(ctx.name, SymbolTableNode(GDEF, info))
#     print("hoi")
#     return


def plugin(version: str):
    return CustomPlugin

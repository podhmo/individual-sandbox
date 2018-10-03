from typing import Optional, Callable

from mypy.plugin import Plugin, FunctionContext
from mypy.types import Type


class MyPlugin(Plugin):
    def get_function_hook(self, fullname: str) -> Optional[Callable[[FunctionContext], Type]]:
        # 00foo.foo
        if fullname.endswith(".foo"):
            return foo_call_callback
        return None


def foo_call_callback(ctx: FunctionContext) -> Type:
    # import pdb; pdb.set_trace()
    if ctx.context.args[0].value.isdigit():
        return ctx.api.named_generic_type('builtins.int', [])
    return ctx.default_return_type


def plugin(version):
    return MyPlugin

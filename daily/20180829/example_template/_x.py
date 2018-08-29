xs = """
__add__
__radd__
__mul__
__rmul__
__div__
__rdiv__

__truediv__
__rtruediv__
__floordiv__
__rfloordiv__

__mod__
__rmod__
__pos__
__neg__
__call__

__getitem__
__lt__
__le__
__gt__
__ge__
__int__

__float__
__complex__
__pow__
__rpow__
__sub__
__rsub__
"""

from prestring.python import Module
m = Module()
with m.scope():
    for x in xs.strip().split("\n"):
        if not x.strip():
            continue
        with m.def_(x, "self", "*args", "**kwargs"):
            m.stmt(f'return self.__getattr__({x!r})(*args, **kwargs)')
print(m)

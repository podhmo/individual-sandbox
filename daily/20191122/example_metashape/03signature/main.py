import typing as t
from fastapi.dependencies.utils import get_typed_signature
from handofcats.accessor import _getfullargspec


class Context:
    pass


def f(ctx: Context, x: int, *, y: t.Optional[int] = None, z: "int") -> int:
    return 0


print(get_typed_signature(f))
print(_getfullargspec(f))

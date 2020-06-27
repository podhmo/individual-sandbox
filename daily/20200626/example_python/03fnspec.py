import typing as t
import typing_extensions as tx
from egoist.internal._fnspec import fnspec


T = t.TypeVar["T"]

# SupportsResolve
class Resoleable(tx.Protocol[T]):
    def resolve(self, d: t.Dict[str, t.Any]):
        ...


def hello(name: str:Query[str]):
    pass


print(fnspec(hello))

import typing as t
import typing_extensions as tx
from egoist.internal._fnspec import fnspec


class Query:
    pass


def hello(name: tx.Annotated[str, Query]):
    pass


print(fnspec(hello))
print(fnspec(hello).arguments[0][1].__metadata__)
print(t.get_args(fnspec(hello).arguments[0][1]))

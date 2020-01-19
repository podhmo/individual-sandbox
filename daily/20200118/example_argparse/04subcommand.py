import typing as t
from handofcats import as_subcommand


@as_subcommand
def f(*, name: str, y: str, z: t.Optional[str] = None, verbose: bool = True):
    r = (name, y, z)
    return r, [type(x) for x in r]


@as_subcommand
def g(*, name: str, i: int, z: t.Optional[int] = 100, verbose: bool = True):
    r = (name, i, z)
    return r, [type(x) for x in r]


as_subcommand.run()

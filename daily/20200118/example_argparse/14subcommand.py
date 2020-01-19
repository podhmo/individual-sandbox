import typing as t


def f(*, name: str, y: str, z: t.Optional[str] = None, verbose: bool = True):
    r = (name, y, z)
    print(r, [type(x) for x in r])


def g(*, name: str, i: int, z: t.Optional[int] = 100, verbose: bool = True):
    r = (name, i, z)
    print(r, [type(x) for x in r])

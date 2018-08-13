import typing as t
import inspect


def f(
    x: int,
    y: int,
    *,
    z: int,
    i: t.Optional[int] = None,
    j: t.Optional[int] = None,
):
    return (x, y, z, i, j)


spec = inspect.getfullargspec(f)
print(spec.annotations)
# {'x': <class 'int'>, 'y': <class 'int'>, 'z': <class 'int'>, 'i': typing.Union[int, NoneType], 'j': typing.Union[int, NoneType]}

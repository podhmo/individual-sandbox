import typing as t


def f(x, *args: int, **kwargs: t.Optional[int]):
    pass


import inspect
from prestring.python.transform import transform_string
print(transform_string(inspect.getsource(f)))

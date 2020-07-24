from typing import get_type_hints


def f(x: int, *, y: int) -> str:
    pass


print(get_type_hints(f))
# {'x': <class 'int'>, 'y': <class 'int'>, 'return': <class 'str'>}

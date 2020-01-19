import typing as t
import inspect


def foo(x: int, y: int) -> t.List[int]:
    return [x, y]


print(inspect.getcallargs(foo, 10, 20))
# {'x': 10, 'y': 20}
print(inspect.getcallargs(foo, 10, 20, 30))
#     raise TypeError("%s() takes %s positional argument%s but %d%s %s given" %
# TypeError: foo() takes 2 positional arguments but 3 were given

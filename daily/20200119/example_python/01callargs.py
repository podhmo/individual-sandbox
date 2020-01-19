import typing as t
import inspect


def add(x: int, y: int) -> t.Dict[str, t.Any]:
    ans = x + y
    return {"q": f"{x} + {y} = ?", "a": ans}


inspect.getcallargs(add, *[10, 20])
try:
    inspect.getcallargs(add, *[10, 20, 30])
except TypeError as e:
    print(repr(e))
    print(f"\texpected signature is {inspect.signature(add)}")

# TypeError('add() takes 2 positional arguments but 3 were given')
# 	expected signature is (x: int, y: int) -> Dict[str, Any]

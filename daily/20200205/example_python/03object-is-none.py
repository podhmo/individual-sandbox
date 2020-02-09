def foo(x: object) -> str:
    return str(x)


print(foo(10))
print(foo(object()))
print(foo(None))

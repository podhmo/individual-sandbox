def foo(x=None):
    def _closure():
        nonlocal x
        print("@", x)
        x = x or 10
        return x

    return _closure


print(foo()())
print(foo(x=20)())
print(foo()())

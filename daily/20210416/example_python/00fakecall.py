import inspect


def hello(*, name: str) -> None:
    raise NotImplementedError(hello)


# hello(name="world")
# TypeError: hello() got an unexpected keyword argument 'nam'
print(inspect.getcallargs(hello, nam="foo"))

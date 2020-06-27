from egoist.internal._fnspec import fnspec


def hello(name: str):
    pass


print(fnspec(hello))

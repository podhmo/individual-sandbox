import typing as t
import inspect


def hello(name: str, *, memo: t.Optional[str] = None) -> None:
    pass


print(inspect.getcallargs(hello, ("foo", )))

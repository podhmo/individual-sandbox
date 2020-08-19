import typing as t


def deco0(fn: t.Callable[..., t.Any]) -> t.Callable[..., t.Any]:
    return fn


@deco0
def fn0(name: str) -> None:
    pass


reveal_type(fn0)


CallT = t.TypeVar("CallT", bound=t.Callable[..., t.Any])


def deco1(fn: CallT) -> CallT:
    return fn


@deco1
def fn1(name: str) -> None:
    pass


reveal_type(fn1)


def deco2(fn: t.Callable[..., t.Any]) -> t.Callable[..., t.Any]:
    return fn


@deco2
def fn2(name: str) -> None:
    pass


reveal_type(fn2)

import typing as t
import typing_extensions as tx
from prestring.go import Module

# types
T_co = t.TypeVar("T_co", covariant=True)


class Config:
    __gomodule__ = "github.com/podhmo/appkit/conf"


class Error:
    """go's error"""

    pass


Cleanup = t.Callable[[], None]


class Provider1(tx.Protocol[T_co]):
    def __call__(self, c: Config) -> T_co:
        ...


class Provider2(tx.Protocol[T_co]):
    def __call__(self, c: Config) -> t.Union[T_co, Error]:
        ...


class Provider3(tx.Protocol[T_co]):
    def __call__(self, c: Config) -> t.Union[T_co, Cleanup, Error]:
        ...


class Provider1WithDeps(tx.Protocol[T_co]):
    def __call__(self, c: Config, *args: t.Any) -> T_co:
        ...


class Provider2WithDeps(tx.Protocol[T_co]):
    def __call__(self, c: Config, *args: t.Any) -> t.Union[T_co, Error]:
        ...


class Provider3WithDeps(tx.Protocol[T_co]):
    def __call__(self, c: Config, *args: t.Any) -> t.Union[T_co, Cleanup, Error]:
        ...


Provider = t.Union[
    Provider1[T_co],
    Provider2[T_co],
    Provider3[T_co],
    Provider1WithDeps[T_co],
    Provider2WithDeps[T_co],
    Provider3WithDeps[T_co],
]


def gen(m: Module, c: str, provider: Provider[T_co]) -> Module:
    tdict = t.get_type_hints(provider)

    ret_type = tdict.pop("return")
    origin = t.get_origin(ret_type)
    if origin and issubclass(origin, tuple):
        ret_args = t.get_args(ret_type)
    else:
        ret_args = [ret_type]

    n = len(ret_args)

    if n == 1:
        # t.Callable[[Config, ...], Object]
        m.stmt("x := {}({})", provider.__name__, c)
    elif n == 2:
        # t.Callable[[Config, ...], t.Tuple[Object, Error]]
        pass
    elif n == 3:
        # t.Callable[[Config, ...], t.Tuple[Object, Cleanup, Error]]
        pass
    else:
        raise ValueError(f"unexpected return type: {ret_type}")
    return m

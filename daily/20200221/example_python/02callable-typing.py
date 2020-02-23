import typing as t
import typing_extensions as tx

T_co = t.TypeVar("T_co", covariant=True)


class Config:
    __gomodule__ = "github.com/podhmo/appkit/conf"


class Error:
    """go's error"""

    pass


class Cleanup:
    """func()"""

    pass


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


class Foo:
    __gomodule__ = "github.com/podhmo/appkit/foo"


def FromConfig(c: Config) -> Foo:
    pass


provider: Provider[Foo] = FromConfig
# if t.TYPE_CHECKING:
#     reveal_type(provider)
#     reveal_type(FromConfig)

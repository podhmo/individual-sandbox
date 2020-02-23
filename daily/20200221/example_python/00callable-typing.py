import typing as t
import typing_extensions as tx

T_co = t.TypeVar("T_co", covariant=True)


class Config:
    __gomodule__ = "github.com/podhmo/appkit/conf"


class Provider1(tx.Protocol[T_co]):
    def __call__(self, c: Config, *args: t.Any) -> T_co:
        ...


class Foo:
    __gomodule__ = "github.com/podhmo/appkit/foo"


def FromConfig(c: Config, *deps: t.Any) -> Foo:
    pass


provider: Provider1[Foo] = FromConfig
# if t.TYPE_CHECKING:
#     reveal_type(provider)
#     reveal_type(FromConfig)

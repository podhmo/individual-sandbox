import typing as t
from appkit import Config


class Foo:
    __gomodule__ = "github.com/podhmo/appkit/foo"


def FromConfig(c: Config) -> Foo:
    pass


print(t.get_type_hints(FromConfig))

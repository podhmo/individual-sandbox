import typing as t
from appkit import Config, Error, Cleanup, gen, Symbol
from prestring.go import Module


class Foo:
    __gomodule__ = "github.com/podhmo/appkit/foo"


def FromConfig(c: Config) -> Foo:
    pass


c = Symbol("c", Config)
m = Module()

with m.func("run", return_="error"):
    m.stmt("filename := *config")
    m.stmt("c, err := conf.LoadConfig(filename)")
    with m.if_("err != nil"):
        m.return_("err")
    m = gen(m, c, FromConfig)
    m.stmt("fooOb := foo.FromConfig(c)")
    m.return_("use(fooOb)")
print(m)

from prestring.codeobject import CodeObjectModule
from go import Module
import appkit

m = Module()
co = CodeObjectModule(m, assign_op=":=")

conf = co.import_("github.com/podhmo/apikit/conf")
foo = co.import_("github.com/podhmo/apikit/foo")


def FromConfig(c: conf.Config) -> foo.Foo:
    pass


with m.func("run", return_="error"):
    filename = co.let("filename", "*config")

    fooOb = co.symbol("fooOb")
    use = co.symbol("use")

    c, err = co.letN(("c", "err"), conf.LoadConfig(filename))
    with m.if_(f"{err} != nil"):
        m.return_(f"{err}")

    fooOb = appkit.build(co, "fooOb", c, FromConfig)
    m.return_(f"{use}({fooOb})")
print(m)

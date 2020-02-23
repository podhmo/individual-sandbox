from prestring.codeobject import CodeObjectModule
from go import Module


m = Module()
co = CodeObjectModule(m, assign_op=":=")

conf = co.import_("github.com/podhmo/appkit/conf")
foo = co.import_("github.com/podhmo/appkit/foo")

with m.func("run", return_="error"):
    filename = co.let("filename", "*config")
    use = co.symbol("use")

    c, err = co.letN(("c", "err"), conf.LoadConfig(filename))
    with m.if_(f"{err} != nil"):
        m.return_(err)

    fooOb = co.let("fooOb", foo.FromConfig(c))
    m.return_(use(fooOb))
print(m)

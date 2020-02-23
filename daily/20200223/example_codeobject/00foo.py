from prestring.python import Module

m = Module()
with m.def_("foo"):
    m.return_("'foo'")
print(m)

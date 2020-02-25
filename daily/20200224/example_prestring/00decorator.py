from prestring.python import Module, Symbol

m = Module()
retry_ = Symbol("retry")

m.stmt("@{}", retry_)
with m.def_("foo"):
    m.stmt("send(10)")
print(m)

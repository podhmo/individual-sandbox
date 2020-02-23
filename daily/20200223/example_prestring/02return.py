from prestring.python import Module
m = Module()
m.return_("foo")
m.return_("foo {}", "x")
m.return_("foo", await_=True)
print(m)

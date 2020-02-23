from prestring.python import Module

m = Module()
with m.def_("hello", "name") as hello:
    m.stmt("print('hello')")

m.stmt(hello("foo"))
m.stmt(hello("bar"))

print(m)
